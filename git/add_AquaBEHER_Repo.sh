#!/bin/bash 
################################################################################################################################################################################
#                                                                                                                                                                              #
# ***** Adding locally hosted AquaBEHER code to GitHub                                                                                                                         #
#                                                                                                                                                                              #
################################################################################################################################################################################

# Changing Your Git Author Identity
# Changing Your Committer Name & Email Globally

 git config --global user.name "RobelTakele" 
 
 git config --global user.email "takelerobel@gmail.com"


# ***** Initialize the local directory as a Git repository. By default the initial branch is called master.

 git init -b main

# ***** Stage and commit all the files in your project. 

 git add . && git commit -m "initial commit"

# ***** Authorise GitHub CLI: you need to authorise it with your GitHub account.

 gh auth login # ?????????
 
# ***** Now, ythe directory is Git ready and your terminal is authorised to make changes to your GitHub account.  
# The command below will create a repository in GitHub called "My-NewRepo".  It will be a public repository.  
# The directory you are in will be source for that GitHub repository and the push the files in the directory to it. 

 gh repo create my-newrepo --public --source=. --remote=upstream --push  # *** Your directory is now Git initiated, you have a GitHub repository to store your project and the files are stored there.

# ***** Configure the remote repository, add the URL for the remote repository where your local repository will be pushed.

git remote add origin  <REMOTE_URL> 

# ***** Sets the new remote

git remote -v

# ***** PPush the files to the remote repository. ush the changes in your local repository to GitHub.com. // # Pushes the changes in your local repository up to the remote repository you specified as the origin

git push -u origin main


# ***** You've now successfully created your local folder into a Git folder and configured the remote repository.  Any changes you make locally can now be easily pushed to your remote repository.

################################################################################################################################################################################
################################################################################################################################################################################
################################################################################################################################################################################
