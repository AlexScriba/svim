# Contributing to svim

:tada::+1: First off, thanks for taking the time to contribute! :tada::+1:

It might not be obvious but we appreciate all forms of contribution.

The following is a set of guidelines for contributing to [svim](https://github.com/AlexScriba/svim). These are mostly guidelines, not rules. Use your best judgment. Feel free to propose changes to this document in a pull request.

> **Important note:** Make sure you make PRs to `dev` branch, not `main`. See [below](#making-a-pull-request) for more information.

#### Table of Contents

- [How Can I Contribute?](#how-can-i-contribute)
  - [Reporting bugs](#reporting-bugs)
  - [Suggesting enhancements/features](#suggesting-enhancementsfeatures)
  - [Making a pull request](#making-a-pull-request)
  - [Get an issue assigned to you](#get-an-issue-assigned-to-you)
- [Github Branching Model](#github-branching-model)

## How Can I Contribute?

### Reporting bugs

1. Before submitting a bug report, make sure to do a cursory search on [issues](https://github.com/AlexScriba/svim/issues) to see if it's already reported. If it's already reported, add a comment under the issue thread instead of opening a new one.

2. Use clear and descriptive title.

3. Include in the body of the issue:

   - **Expected behavior**: What do you expect should happen?
   - **Actual behavior**: What actually happened and why it's a problem?
   - **Steps to reproduce the problem**. Be very specific. Give example code block. Other contributors want to run it in their device to make sure they see what you saw. Having detailed steps and examples can make it easier to demonstrate and track down a problem.
   - **Version information**: What version of Python you're using? What version of svim?

   You can include screenshots/GIFs, if relevant.

Additionally, you can also confirm other people's bug report by running their provided code and steps in your local machine and see if the same problem shows up. Every test helps, especially if your device setup is different (i.e. has different OS or Python version) from the original bug report.

### Suggesting enhancements/features

1. Before submitting a feature suggestion, make sure to do a cursory search in [issues](https://github.com/AlexScriba/svim/issues) to see if it's already suggested.

2. Use clear and descriptive title.

3. Lay out the details of your suggestion in the body issue. Make sure to also:

   - Describe the current behavior and explain what would you like to see instead.
   - Explain why your suggestion would be useful for svim users.

### Making a pull request

You can also make a pull request to fix an existing bug or add a feature.

Unsure where to begin contributing to svim? You can start by looking through these `first-contribution`, `beginner`, `help-wanted` issues:

- `first-contribution` issues should only require a few lines of code or are improvements on the documentation.
- `beginner` issues are a step up, and may involve a couple of methods/tests.
- `help-wanted` issues are slightly trickier.

One very important thing is to make sure you only make PR's to the `dev` branch and not the `main` branch. The main branch is stable, and `dev` branch changes will be merged into `main` periodically once it's confirmed that they don't have any breaking changes.

### Get an issue assigned to you

If you find an issue that you'd like to tackle - get it assigned to yourself by commenting on it with:

```
Hold my beer, I got this
```

## Setting Up Local Development Environment

### Getting a local copy

1. Fork this repository.

2. Clone your fork to your local device.

3. Set your fork as `origin` remote. This is usually done automatically if you're using `git clone` command. To do it manually:

   ```sh
   git remote add origin URL_OF_YOUR_FORK
   ```

4. Set original repository as `upstream` remote.

   ```sh
   git remote add upstream https://github.com/AlexScriba/svim.git
   ```

5. Pull from original repository to make sure you're synced up.

   ```sh
   git pull upstream dev:dev
   ```

   You may want to do it once in a long while to make sure your local dev branch is in sync with the `upstream` remote.

6. Checkout the `dev` branch, and make a new branch from there to make changes.

   ```sh
   git checkout dev
   git checkout -b my-feature-branch
   ```

### Pushing changes and opening a pull request

1. Push the changes to your forked repository.

2. Return to your forked repository on Github and click on `compare and pull request` to begin the PR. **Make sure the base branch is `dev`, not `main`.**

3. Describe your PR, submit it and wait for it to be merged! You may be required to do additional work or changes before it is merged.

## Github Branching Model

svim branches are created using a Github branching model. In this branching model, each branch serves a purpose and offers team members a shared undestanding of the branching system.
