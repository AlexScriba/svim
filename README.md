<div align=center

</br>

# Svim

## A Simple Vim Wrapper for easy recent project access.

</div>

_Don't forget to star Svim repository if you found it helpful_

#### Table of Contents

[Install svim](#install-it-here)

[Getting started](#getting-started)

[Contributing and submitting PR's](#contributing-and-submitting-pull-requests)

[Semantic Versioning System](#semantic-versioning-system)

[Attributions](#world-air-quality-index-and-epa-attribution)

[License and TOS](#license-and-terms-of-service)

[Contributors](#contributors)

## Install it here!

Since svim is still pre-release, the installation process is still quite manual.
For now, it has only been tested on MacOS, but should in theory work on Linux as well.

1. Download or Clone the project files.
2. Make sure you have cabal (a haskell build tool) installed.
3. In the project directory, run:
   ```shell
   cabal build svim
   ```
4. Fetch the executable from the dist-newstyle/build/ directory. (Keep going down the hierarchy until you find the executable called 'svim').
5. Move that to your /usr/local/bin/ directory (For MacOS).

The executable should be available globally.

## Getting started

### Setting Preffered Editor

To use svim set your preferred editor command:

```shell
svim --set-editor <editor-command>
```

It defaults to neovim (nvim). As an example of setting that:

```shell
svim --set-editor nvim
```

### Opening a project

To open a project using svim, simply type svim followed by the relative path:

```shell
svim ~/.config/nvim
```

This will open the project with your chosen editor, and save that file path for future use.

### Opening a recent project

Svim keeps track of your most recently opened projects. Once projects are saved, simply type:

```shell
svim
```

and a interactive menu pops up allowing you to choose from your recently opened projects

### Saving Favourites

You can also save some favourite projects that you access often.

Type svim followed by the -s flag, the name you want to call it, and the filepath.

```shell
svim -s nvim_config ~/.config/nvim/
```

You can then access these with the -f flag:

```shell
svim -f
```

this opens the interactive menu to let you select your project

You can also run to delete a favourite:

```shell
svim -d
```

This will guide you through deleting a favourite.

## Contributing and submitting Pull requests

**We love PR's!**

Take a look at the [CONTRIBUTING.md](https://github.com/AlexScriba/svim/blob/main/CONTRIBUTING.md) file for details on how to go about this!

## Semantic Versioning System

Svim uses a semantic versioning system to increment its release version number. Using this model, changes in version numbers can help indicate the meaning of modified code for each version.

## LICENSE and Terms of Services 📰

Svim is licensed under the MIT License. See the [LICENSE.md](https://github.com/AlexScriba/svim/blob/main/LICENSE.md) for more details.

## Contributors

Contributions of any kind are welcome! These are our amazing contributors :)

#### _Created by [Alexander Scriba](https://github.com/AlexScriba)_
