# EMP Mode for Emacs

Control MPRIS2 compatible music players from Emacs

## Usage

To get this working, you must be running systemd/dbus on a GNU/Linux
distro. First, launch your music application
(Rhythmbox/Amarok/Spotify-Linux/etc.).
Then, in emacs, load up the minor mode with ``M-x emp-mode``.
From there, use the interactive functions:

| Command        | Default Binding | Effect              |
| -------------- | --------------- | ------------------  |
| emp-next       | ``C-c n``       | Plays next song     |
| emp-prev       | ``C-c p``       | Plays previous song |
| emp-toggle     | ``C-c m``       | Toggles play/pause  |
| emp-play       | ``C-c i``       | Plays current song  |
| emp-pause      | ``C-c u``       | Pauses music player |
| emp-stop       | ``C-c s``       | Stops music player  |
| emp-volume-up  | None            | Raise volume by 10% |
| emp-volume-down| None            | Lower volume by 10% |

## Installation

WIP

## Contributing

Why not? Create a pull request. There are no external dependencies.

## Authors
* [Griffin Mareske](mailto:gmareske@gmail.com) - gmareske@gmail.com

## License
This project is licensed under the MIT License. For more details, see LICENSE
