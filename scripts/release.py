#!/usr/bin/env python3
"""
Release and Version Bump Automation Script for datastructs-fortran.

Usage:
    python3 scripts/release.py patch       # 0.4.5 -> 0.4.6
    python3 scripts/release.py minor       # 0.4.5 -> 0.5.0
    python3 scripts/release.py major       # 0.4.5 -> 1.0.0
    python3 scripts/release.py 0.5.0       # Explicit target version
    python3 scripts/release.py patch --dry-run
"""

import sys
import re
import subprocess
from pathlib import Path

ROOT_DIR = Path(__file__).resolve().parent.parent
FPM_TOML = ROOT_DIR / "fpm.toml"
FORD_MD = ROOT_DIR / "ford.md"
FORMAL_MD = ROOT_DIR / "docs" / "formal.md"
INDEX_MD = ROOT_DIR / "docs" / "index.md"

def run_cmd(cmd, check=True):
    result = subprocess.run(cmd, cwd=ROOT_DIR, capture_output=True, text=True)
    if check and result.returncode != 0:
        print(f"Error running command: {' '.join(cmd)}")
        print(result.stderr)
        sys.exit(result.returncode)
    return result.stdout.strip()

def get_current_version():
    content = FPM_TOML.read_text(encoding="utf-8")
    m = re.search(r'^version\s*=\s*"([^"]+)"', content, re.MULTILINE)
    if not m:
        raise RuntimeError(f"Could not find version string in {FPM_TOML}")
    return m.group(1)

def compute_next_version(current_version, bump_type):
    parts = current_version.split(".")
    if len(parts) != 3:
        raise ValueError(f"Version '{current_version}' is not in semantic versioning format (X.Y.Z)")
    
    major, minor, patch = map(int, parts)
    
    if bump_type == "patch":
        patch += 1
    elif bump_type == "minor":
        minor += 1
        patch = 0
    elif bump_type == "major":
        major += 1
        minor = 0
        patch = 0
    elif re.match(r'^\d+\.\d+\.\d+$', bump_type):
        return bump_type
    else:
        raise ValueError(f"Invalid bump type or version '{bump_type}'. Choose 'patch', 'minor', 'major' or X.Y.Z.")
        
    return f"{major}.{minor}.{patch}"

def get_latest_git_tag():
    out = run_cmd(["git", "tag", "-l", "v*", "--sort=-v:refname"], check=False)
    if not out:
        return None
    tags = out.splitlines()
    return tags[0] if tags else None

def get_commits_since_tag(tag):
    if tag:
        range_spec = f"{tag}..HEAD"
        commits = run_cmd(["git", "log", range_spec, "--pretty=format:%h %s", "--no-merges"], check=False)
    else:
        commits = run_cmd(["git", "log", "--pretty=format:%h %s", "--no-merges"], check=False)
    return commits.splitlines() if commits else []

def format_release_notes(version, commits):
    notes = [f"Release v{version}\n"]
    if commits:
        notes.append("Commits included in this release:")
        for c in commits:
            notes.append(f"- {c}")
    else:
        notes.append("Maintenance and performance improvements.")
    return "\n".join(notes)

def update_version_in_files(new_version):
    files_to_commit = []

    # 1. fpm.toml
    if FPM_TOML.exists():
        content = FPM_TOML.read_text(encoding="utf-8")
        new_content = re.sub(
            r'^version\s*=\s*"[^"]+"',
            f'version = "{new_version}"',
            content,
            flags=re.MULTILINE
        )
        FPM_TOML.write_text(new_content, encoding="utf-8")
        files_to_commit.append(str(FPM_TOML.relative_to(ROOT_DIR)))
        print(f"[✓] Updated {FPM_TOML.relative_to(ROOT_DIR)} to {new_version}")

    # 2. ford.md
    if FORD_MD.exists():
        content = FORD_MD.read_text(encoding="utf-8")
        new_content = re.sub(
            r'^version:\s*[\d\.]+',
            f'version: {new_version}',
            content,
            flags=re.MULTILINE
        )
        FORD_MD.write_text(new_content, encoding="utf-8")
        files_to_commit.append(str(FORD_MD.relative_to(ROOT_DIR)))
        print(f"[✓] Updated {FORD_MD.relative_to(ROOT_DIR)} to {new_version}")

    # 3. docs/formal.md
    if FORMAL_MD.exists():
        content = FORMAL_MD.read_text(encoding="utf-8")
        new_content = re.sub(
            r'^version:\s*[\d\.]+',
            f'version: {new_version}',
            content,
            flags=re.MULTILINE
        )
        FORMAL_MD.write_text(new_content, encoding="utf-8")
        files_to_commit.append(str(FORMAL_MD.relative_to(ROOT_DIR)))
        print(f"[✓] Updated {FORMAL_MD.relative_to(ROOT_DIR)} to {new_version}")

    # 4. docs/index.md (fpm example tag = "vX.Y.Z")
    if INDEX_MD.exists():
        content = INDEX_MD.read_text(encoding="utf-8")
        new_content = re.sub(
            r'tag\s*=\s*"v[\d\.]+"',
            f'tag = "v{new_version}"',
            content
        )
        INDEX_MD.write_text(new_content, encoding="utf-8")
        files_to_commit.append(str(INDEX_MD.relative_to(ROOT_DIR)))
        print(f"[✓] Updated {INDEX_MD.relative_to(ROOT_DIR)} to v{new_version}")

    return files_to_commit

def main():
    if len(sys.argv) < 2 or sys.argv[1] in ("-h", "--help"):
        print(__doc__)
        sys.exit(0)

    bump_arg = sys.argv[1].lower()
    dry_run = "--dry-run" in sys.argv
    no_commit = "--no-commit" in sys.argv

    curr_version = get_current_version()
    next_version = compute_next_version(curr_version, bump_arg)
    new_tag = f"v{next_version}"

    latest_tag = get_latest_git_tag()
    commits = get_commits_since_tag(latest_tag)
    release_notes = format_release_notes(next_version, commits)

    print("========================================")
    print(f" Datastructs-Fortran Release Automation ")
    print("========================================")
    print(f" Current version:  {curr_version}")
    print(f" Target version:   {next_version} ({new_tag})")
    print(f" Base tag:         {latest_tag or '(initial)'}")
    print(f" Commits found:    {len(commits)}")
    print("----------------------------------------")
    print("Release Notes:")
    print(release_notes)
    print("----------------------------------------")

    if dry_run:
        print("[DRY-RUN] No changes were written.")
        sys.exit(0)

    # 1. Update version across fpm.toml, ford.md, formal.md, docs/index.md
    files_to_commit = update_version_in_files(next_version)

    if no_commit:
        print("[!] --no-commit specified. Skipping git commit and tag.")
        sys.exit(0)

    # 2. Git commit
    run_cmd(["git", "add"] + files_to_commit)
    commit_msg = f"chore(release): bump version to v{next_version}"
    run_cmd(["git", "commit", "-m", commit_msg])
    print(f"[✓] Created git commit: '{commit_msg}'")

    # 3. Git annotated tag
    run_cmd(["git", "tag", "-a", new_tag, "-m", release_notes])
    print(f"[✓] Created annotated git tag: {new_tag}")

    print("\n[SUCCESS] Release complete!")
    print("To push changes and tags to remote, run:")
    print(f"    git push origin main && git push origin {new_tag}")

if __name__ == "__main__":
    main()
