# brocade
the Fabric's presentation server

## Overview

The Fabric's clients use a common presentation server that renders
scenes on screens, processes user events, and communicates with
autonomous processes that handle gameplay and game logic. This design
reduces coupling among the graphics and sound subsystems and the game
logic, and facilitates the development of tools because the tools can
be written to talk to the same API that the game client uses, rather
than requiring their own tighly-coupled APIs to provide rendering and
presentation.

The Fabric client is structured as an autonmous presentation server on
one hand, and a set of client programs that communicate with the
server on the other hand. The basic idea is similar to the
organization of the X window system, but the actual APIs are much
simpler and more efficient, both because there is no requirement for
the presentation server to support arbitrary clients and network
connectivity, and because the presentation server uses the far simpler
9P protocol.

This is a preliminary document that sketches the high-level design of
the brocade server and its APIs. Expect details to change as we
discover improvements to the organization of the API.

## The brocade filesystem

As in all 9P applications, brocade presents its services as a logical
filesystem. The top levels of the filesystem look like this:

    /
    +--------config/
    +--------state/
             +--------current/
             +--------auth/
                      +--------request
                      +--------response
                      +--------session
             +--------character/
                      +--------characters/
                      +--------create
                      +--------current/
                      +--------delete
                      +--------pick
             +--------scene/
                      +--------current/
                      +--------pick
                      +--------scenes/
    +--------w/
             +--------$windid/event

The `config` directory contains files that represent the configuration
of the presentation server. Configuration data can be read by fetching
the contents of the files, and writable files may be overwritten to
update the configuration.

The `state` directory represents the **current state** of the
presentation application. The **current state** means which of several
modes the application can be in, including:

- waiting for a player to log in
- waiting for a player to choose, create, or delete a character
- waiting for the player to choose a scene in which to enter the game
- playing the game, and therefore rendering a scene

From the point of view of client programs, all of these activities are
performed through operations on the brocade filesystem. For example,
to log in to the game a user enters credentials and clicks a button;
the client program writes the credentials to brocade's
/state/auth/request file and then waits for /state/auth/response to be
written. If /state/auth/response appears then the client copies its
contents to the session file; if not, then the client resets after a
timeout.

Brocade always has a current state, represented by the directory
/state/current/. /state/current cotains a link to one of the other
state directories. When the server initially starts up, the current
state is represented by /state/current/auth. Once authentication and
authorization have completed, and a valid session file has been
written in /state/auth, th state changes to
/state/current/character. Once the player chooses a character and
signals the client that it's ready to play, it changes to
/state/current/scene.

The game world is organized into a set of scenes, each one
representing a place the play can be in the world. The directory
/state/current/scene/ contains a listing of all possible scenes, plus
a "current" directory. The contents of /state/current/scene/current
are the currently-active scene--the scene in which the player appears
when gameplay starts. When a player travels to a different scene,
/state/current/scene/current is updated to reflect the change of
location.

The `w` directory contains a list of subdirectories, one for each
active window. During game play there is normally only a single window
active, but if the client poses a dialog, the dialog window will
appear in /w, and during states other than `scene`, there may be more
than one window present.

Each active window is presented in /w under its **window identifier**,
an ID string generated for it when the window is created. No two
windows in a single session ever share the same window identifier.

There is a subdirectory in /w for each active window, and in that
directory a file named "event". The file /w/$windowid/event is a log
of window events for the window identified as $windowid. A client
program can fetch the latest window events by reading the tail of that
log. Moreover, it can post events to the window by appending lines of
text to the event log.

The format of the event log looks like this:

    (event sender kind xcoord ycoord time flags text)

## Scenes

Each scene directory provides a view into an active scene in the
running game. The Fabric runs on a scene graph engine, which is
convenient for the purposes of the brocade server. It presents the
graph of scene nodes as a filesystem. Each node in the scene graph is
a node in the logical filesystem. The root node of the scene graph is
represented by the scene directory. Each child node of the root node
is represented by a directory or file in the scene directory.

Here's an example of a simple Fabric scene as it appears in the
brocade filesystem:

    scene/
    +--------iulQwBjPrFaPyHM+5349TaGD3qg=/
             +--------gui/
                      +--------chatbox/
                      +--------node-banner/
                      +--------action-bars/
                      +--------minimap/
             +--------player/
             +--------node/

The directory named `iulQwBjPrFaPyHM+5349TaGD3qg=` represents the root
node of the scene graph. Each subdirectory is a node in the scene
graph that is a child node of the root. Client programs can read
descriptive data about the scene graph and the states of its nodes
from this filesystem, and can write data to control files in the
directories representing each node in order to control their behavior.

