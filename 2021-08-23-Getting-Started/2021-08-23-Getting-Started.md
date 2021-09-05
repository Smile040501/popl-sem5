# Getting Started.

DEADLINE: 30 Aug 2021, Monday 11:59 hrs.

This lab session is meant to help you setup your machine and account
so that you can easily submit your assignment for this course. We will
assume that you have the following.

1. An account in the lab.

2. A laptop for work at your hostel. Please install a GNU/Linux
   distribution (e.g. Debian/Ubuntu etc). We will give commands to install
   stuff assuming that you are working with Debian or Ubuntu.


You will need to bring your laptop to the lab as well.

## Task 1 : Setting up the repository

1. Install necessary software on your laptop (the lab machines will
   hopefully have them already). You might already know about `git`
   and `emacs`, checkout what the other software does (Google search).

	    sudo apt update  # update the package list
        sudo apt install git emacs magit tig gitk # install emacs git and helper programs.
        sudo apt install smlnj smlnj-doc # install standard ML interpreter
        sudo apt install mlton           # install standard Ml compiler.


2. On your local machine (both laptop and the lab desktop) configure
   git to use your name and email on commit messages.

        git config --global user.name 'Your Name'
		git config --global user.email 'your-email-id@iitpkd.ac.in'


3. Create an account on [gitlab] (use your iitpkd email address for
   registering).

4. Login to your [gitlab] account and _watch_ the [popl
   repository][popl]. If you watch a particular repository on
   gitlab (in our case the [popl repository][popl]) then you will
   be informed (via email) whenever I push changes to the
   repository. This way you will get information about new assignment
   uploads, changes to source code etc.

5. You can also create a _fork_ of the [popl repository][popl] on
   gitlab. A `fork` is an independent copy of the repository that
   is under your ownership (not mine). You can independently make
   changes to it.

6. In your directory hierarchy (either on desktop or laptop), move to
   some subdirectory where you want the copy of the course repository
   and clone it.

        mkdir -p code/git/
        cd code/git
		git clone https://gitlab.com/your-gitlab-account/popl.git
		cd popl # The popl directory now contains the repository
   Notice that in this case, you have cloned _your_ fork of the repository.
   In the lab, you might need to set the http proxy for git. Please figure
   out how this is to be done (google it).

7. The cloning of a repository creates what is known as a _remote_
   called `origin`. If you want to pull changes from my fork without
   bothering to login to your gitlab account you should create a
   _remote_ which we call `upstream` to my fork.

        git remote add upstream https://gitlab.org/piyush-kurur/popl.git
        git remote -v  # shows all remotes for this repo.

8. If you do not want to type passwords all the time, you can use the
   `ssh url` of your repository as the remote. See
   https://piyush-kurur.github.io/posts/2011-06-02-SSH-A-quick-guide.html
   for a quick `ssh` guide.


## Task 2

Go through the [standard ml tutorial][sml-tutorial] available from
this repository and attempt as many exercise as possible.

[gitlab]: <https://gitlab.com> "Bitbucket"
[popl]: <https://gitlab.com/piyush-kurur/popl> "PoPL course repository"
[sml-tutorial]: <../examples/sml/tutorial.sml> "SML tutorial"
