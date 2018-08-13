# emp-mode for Emacs

Control MPRIS2 compatible music players from Emacs

## Installation

WIP 

## Usage

### Prerequisites
- Linux distro using systemd/dbus
- emp-mode installed in your Emacs distribution

### Configuration
In your .emacs file:
```lisp
(require 'emp)
(setq mpris-service "org.mpris.MediaPlayer2.X")
```
where ``X`` is a string that corresponds to the name of the 
music player you want to control. Below is a table of common Linux
music players and their respective names as far as DBus is concerned:

| Player Name | DBus name |
| ----------- | --------- | 
| Rhythmbox   | ``rhythmbox`` |
| Spotify-Linux| ``spotify``  |
| Amarok      |  ``amarok``   |

### Using emp-mode
First, launch your music application
(Rhythmbox/Amarok/Spotify-Linux/etc). Queuing music is not supported
in emp-mode, so you will need to start some music playing through the
music player's interface.
Then, in any emacs buffer, load up the minor mode with ``M-x emp-mode``.
From there, use the interactive functions:

| Command        | Default Binding | Effect              |
| -------------- | --------------- | ------------------  |
| emp-next       | ``M-p n``       | Plays next song     |
| emp-prev       | ``M-p p``       | Plays previous song |
| emp-toggle     | ``M-p m``       | Toggles play/pause  |
| emp-play       | ``M-p i``       | Plays current song  |
| emp-pause      | ``M-p u``       | Pauses music player |
| emp-stop       | ``M-p s``       | Stops music player  |
| emp-volume-up  | None            | Raise volume by 10% |
| emp-volume-down| None            | Lower volume by 10% |

## Contributing
Why not? Create a pull request.

## Authors
* [Griffin Mareske](mailto:gmareske@gmail.com) - gmareske@gmail.com

## License
This project is licensed under the MIT License. For more details, see LICENSE
