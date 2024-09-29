#!/usr/bin/env python3
import requests
import glob
import argparse
import os
import sqlite3
import sys
import subprocess
import logging
import shutil

SESSION_COOKIE_KEY = "session"

logger = logging.getLogger(__name__)

def get_firefox_cookies(domain_filter: str):
    # Written with chatgpt
    """
    Retrieve cookies from Firefox for a given domain.

    Parameters:
    - domain_filter: The domain to filter cookies

    Returns:
    - Dictionary of cookies for the specified domain.
    """
    # Find the Firefox profile directory
    firefox_dir = os.path.expanduser("~/.mozilla/firefox/")
    profiles = glob.glob(firefox_dir + "*.default-release")

    if not profiles:
        raise Exception("No Firefox profile found!")

    profile_path = profiles[0]
    cookies_db_path = os.path.join(profile_path, "cookies.sqlite")

    # Connect to the Firefox cookie database
    conn = sqlite3.connect(cookies_db_path)
    cursor = conn.cursor()

    # Query the cookies for the specified domain
    cursor.execute("""
        SELECT name, value FROM moz_cookies
        WHERE host LIKE ?
    """, (f"%{domain_filter}%",))

    # Fetch and return cookies as a dictionary
    cookies = {name: value for name, value in cursor.fetchall()}

    conn.close()
    return cookies

def grab_session_cookie(cookie_file):
    if os.path.exists(cookie_file):
        logger.info("Cookie exists in cache, retrieving")
        with open(cookie_file, "r") as f:
            return f.read()

    # Gotta read the cookie from Firefox
    logger.info("No cookie in cache, reading from firefox")
    session_cookie = get_firefox_cookies("adventofcode.com")[SESSION_COOKIE_KEY]
    with open(cookie_file, "w+") as f:
        logger.info(f"Caching cookie to {cookie_file}")
        f.write(session_cookie)

    return session_cookie

def get_input(year: int, day: int) -> str:
    cache_root = os.environ.get('XDG_CACHE_HOME', default = None)
    if cache_root is None:
        cache_root = "~/.local/cache"
    cache_dir = os.path.expanduser(f"{cache_root}/anlsh-aoc")

    problem_input_file = f"{cache_dir}/{year}_{day}.txt"

    if os.path.exists(problem_input_file):
        logger.info(f"Problem input already cached as {problem_input_file}")
        with open(problem_input_file, "r") as f:
            return f.read()

    os.makedirs(cache_dir, exist_ok=True)

    cookie_file = f"{cache_dir}/session-cookie"
    cookies = {SESSION_COOKIE_KEY: grab_session_cookie(cookie_file)}

    url = f"https://adventofcode.com/{year}/day/{day}/input"

    response = requests.get(url, cookies=cookies)
    if response.status_code == 200:
        data = response.text.strip()
        with open(problem_input_file, "w+") as f:
            logger.info(f"Caching input to {problem_input_file}")
            f.write(data)
        return data
    elif response.status_code == 400:
        try:
            os.remove(cookie_file)
        except OSError:
            pass
        raise RuntimeError("Could not authenticate with AOC. Please log in " + \
                           "with firefox, then close it, then retry")
    else:
        raise RuntimeError(response)

def solve(year: int, day: int) -> None:
    """
    Find a file to solve the problem and run it.
    """
    input_data = get_input(year, day)
    try:
        files = os.listdir(str(year))
    except OSError as e:
        raise RuntimeError(f"Folder {year} not found: are you running from the repo root?") from e

    files = filter(lambda f: str(day) in f, files)
    # Don't pick up double-digit numbers for single-digit files
    files = filter(lambda f: not any([(str(day) + str(k)) in f for k in range(10)]), files)
    files = [f'{year}/{f}' for f in files]
    files = [f for f in files if os.access(f, os.X_OK)]

    if len(files) == 0:
        raise RuntimeError(f"No candiate executables for specified args! Ensure an executable file containing '{day}' exists under {year}")

    solver_file = files[0]
    solver_process = subprocess.Popen(f"./{solver_file}", stdin=subprocess.PIPE, text=True)
    solver_process.stdin.write(input_data)
    solver_process.stdin.close()
    solver_process.wait()

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Download AoC inputs")
    parser.add_argument("action", type=str, choices=("solve, show, makepy"),
                        help="'solve' to run solution, 'show' to show input")
    parser.add_argument("year", type=int, help="2015, 2016, etc")
    parser.add_argument("day", type=int, help="1, 2, 3, etc")
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable verbose output.')
    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)

    if args.action == "show":
        print(get_input(args.year, args.day))
    elif args.action == "solve":
        solve(args.year, args.day)
    elif args.action == "makepy":
        file_name = f'{args.year}/{args.day}.py' if args.day >= 10 else f'{args.year}/0{args.day}.py'
        if os.path.exists(file_name):
            logging.error(f"{file_name} already exists, will not overwrite")
            exit(1)
        shutil.copy("templates/template.py", file_name)
