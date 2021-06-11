# Commit editor
A simple editor to write commits in. Limits the title to 72 characters so the title is properly separated from the body. This is because Github cuts of titles after 72 characters and displays the rest as body. Keep your commit title short and add `[See details]` at the end of it, then describe in detail what you changed in the body. You can find some examples in the commit history of this repository.

# Dependencies
- gtk3
- gtk2hs-buildtools

# Install
```
stack build --copy-bins
```

Then set `hs-commit` as your editor in your `.gitconfig`.

# To-do
- [X] Option to abort commit
- [ ] Show which files are being modified
