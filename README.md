# The Fabric #

## How to Develop ##

All Fabric development is currently done on Mac OS X, though the Fabric itself is portable code that doesn't depend on OS X. The development setup assumes the use of OS X at the moment, though; if you want to develop for the Fabric on Windows or Linux instead, contact [mikel](mailto:mevins@me.com)  to coordinate any changes in the project that might be needed.

### 1. Install Git ###

If git is not already installed on your system, follow the instructions here:

http://git-scm.com/

### 2. Get the Fabric ###

Using git, clone the Fabric sources to your machine. The Fabric's git repo is [Here ](https://bitbucket.org/mikelevins/fabric). If you're going to be pushing changes back to the repo you'll need to contact  [mikel](mailto:mevins@me.com) and give him a public key for ssh that can be added to the git repo to allow you to push.

Put the Fabric repo some place convenient on your development machine.

### 3. Install and Configure Emacs ###

Emacs is an essential part of the Fabric development environment.

#### A. Get Emacs

For Mac OS X the recommended Emacs version is [Aquamacs](http://aquamacs.org/).

Development on other platforms is currently untested. Your best bet is [GNU Emacs](http://www.gnu.org/software/emacs/).

#### B. Configure Emacs ####

Add the following lines to your Emacs startup file ("~/.emacs" or "~/.emacs.d/init.el"):

    (load "~/<fabric path>/fabric/emacs/fabric.el")
    (load "~/<fabric path>/fabric/emacs/quack.el")

You must change the `"<fabric path>"` part of the pathname strings to correctly identify where you put the fabric project. 

#### C. Check the Permissions of the Kawa Script ####

Check to make sure that the `kawa` script has execute permissions.

Emacs uses a script named `fabric/bin/kawa` to launch the Scheme process we use for compiling and running the game engine. Sometimes revision-control software fails to set permissions correctly.  You need to check to ensure that `kawa` has permission to run.

A. Open a terminal window and change directory to 

    ~/<fabric path>/fabric/bin

For the `<fabric path>` portion of the pathname, substitute the directory where you put the Fabric project.

In the terminal window, execute

    ls -l
    
Check the permissions displayed for the `kawa` file. You should see this:

-rwxr-xr-x   1 *username*  *group*  *size* *date* kawa*

...where *username* and  *group* are your username and group, and  *size*, and *date* are whatever they happen to be. If you don't see the 'x' characters in the permissions field on the left, or if the asterisk (*) doesn't appear at the end of the name "kawa", then the script is not executable, and you need to fix it.

In that case, change the permissions by running this command at the terminal prompt:

    chmod +x kawa

The script should now have permission to execute.

#### D. Launch a Session ####

Everything should now be set up and ready to run. The next step is to set the setup to make sure it works.

1. Launch Emacs
2. With an Emacs window active, type Meta-x qfab. (If you don't know what this means, read [this explanation of M-x](https://www.gnu.org/software/emacs/manual/html_node/emacs/M_002dx.html). If everything is set up correctly, you should see a new buffer open in Emacs and display the `#|kawa|#` prompt.

Congratulations; you're ready to develop the. Fabric.


