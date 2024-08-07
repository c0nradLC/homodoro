# homodoro
A very simple terminal application to use the pomodoro technique and keep track of daily tasks.

<p align="center">
  <img alt="example usage of homodoro" src="./media/homodoro.svg">
</p>

## Contents
- [About the project](#about-the-project)
- [Installation](#installation)
- [Usage](#usage)
  - [Commands](#commands)
  - [Tasks file](#tasks-file)
  - [Timers and their alerts](#timers-and-their-alerts)
- [Misc](#misc)

## About the project
The motivation behind this project was to simply get more used to the Haskell syntax and the Functional Programming way of programming, this was started with literally zero previous knowledge about Haskell and FP, I literally discovered stuff while I was doing it so it may be the case that the code is not the best possible but it definitely helped me get my feet wet in the entire Haskell and FP way of thinking.

If you are also new and interested in learning more about Haskell and FP, this might be a good project for you to take a look and probably try implementing some new features or refactoring code that looks bad, either way feel free to use, edit and do whatever you want with this piece of software.

Even though the program has very little functionality and is very simple, I do want to implement some more functionality to it as time goes by, if you are not interested in learning Haskell but want a specific feature to be implemented, feel free to create an issue.

## Installation
Currently only Linux is supported.

You can download the binary through the [Releases](https://github.com/c0nradLC/homodoro/releases). You can run it directly where it was downloaded or put it in a directory present in your `PATH`.

You can also build homodoro with `nix-build` or `cabal build` or run a development shell with `nix-shell`.

## Usage
Just run `homodoro` and you should see the timers and the task list.

Every time the Pomodoro timer reaches 0, the `Pomodoros` counter increases by 1, every 4th `Pomodoro` round ends, instead of switching the focus to `Short Break`, the focus will be switched to `Long Break`, everytime a `Long Break` is finished, the counter that switches to `Long Break` on the 4th `Pomodoro` resets and the cycle starts again.

#### Commands
You can press `c` when not editing or creating a task to see the commands.

- `Shift + Tab` AKA `BackTab` Go to next timer
- `q` Quit application
- `s` Start/stop timer
- `r` Reset timer
- `i/d` Increase/Decrease timer by 1min
- `I/D` Increase/Decrease timer by 10sec
- `t` Add a task
- `e` Edit a task
- `Del` Delete a task
- `Ctrl + C` Change a task's status

Other than the commands that are shown at the bottom of the screen, you can check the [List](https://hackage.haskell.org/package/brick-2.3.1/docs/Brick-Widgets-List.html#g:3) and [Editor](https://hackage.haskell.org/package/brick-2.3.1/docs/Brick-Widgets-Edit.html) commands in the Brick documentation for each of the Widgets.

#### Tasks file
The tasks file is saved at the path set to the `XDG_DATA_HOME` environment variable.
> According to it's [specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html), if that variable is not set or empty, it defaults to $HOME/.local/share on non-Windows systems.

#### Timers and their alerts
When a timer reaches zero an alarm sound should ring and a notification popup should appear.
  
> The popup is shown using the [libnotify](https://hackage.haskell.org/package/libnotify) library
  
> The alarm sound is played using [SDL2](https://hackage.haskell.org/package/sdl2) and [SDL2-mixer](https://hackage.haskell.org/package/sdl2-mixer)

## Misc
Written in Haskell using the [brick](https://github.com/jtdaugherty/brick) library. This was heavily inspired by the other brick programs featured in it's repo's [Featured Projects](https://github.com/jtdaugherty/brick#Featured-Projects).
