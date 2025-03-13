setwd("path/to/your/forked-repo")  # Move into your repo folder
system("git remote add upstream https://github.com/mttinker/SOFA.git") 
system("git fetch upstream")  # Get the latest changes from the original repo
system("git reset --hard upstream/master")  # Sync local repo with upstream
system("git push origin master --force")  # Update your GitHub fork
