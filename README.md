
# Table of Contents

1.  [emacs-sftp](#org0920bc9)
2.  [Installing](#orgfd5b100)
    1.  [Linux dependent environment](#orgf95151f)
    2.  [Windows depends on the environment](#org3e54184)
3.  [Configuration of the sample](#org1280a34)
    1.  [Connection information Settings](#orgb210060)
4.  [Usage](#org4f91cee)


<a id="org0920bc9"></a>

# emacs-sftp

SFTP is the mechanism to use the SCP command to transport files to a remote server 


<a id="orgfd5b100"></a>

# Installing

Introduce sftp.el into your configuration and add it to your configuration 

    (require 'sftp)


<a id="orgf95151f"></a>

## Linux dependent environment

-   scp
-   sshpass

    apt-get install sshpass


<a id="org3e54184"></a>

## Windows depends on the environment

-   pscp

[Download the PSCP tool here and add it to path](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html)


<a id="org1280a34"></a>

# Configuration of the sample


<a id="orgb210060"></a>

## Connection information Settings

Use.dir -locals. El to set the connection information for the current project

    ((nil
      (host ."127.0.0.1")
      (user ."username")
      (password ."password")
      (remote_path ."/www") ;;Remote server engineering path 
      (port ."22")
    ))

Therefore, you need to read the variables and suggest joining before you introduce SFTP

    (setq enable-local-variables :all enable-local-eval t)
      (hack-dir-local-variables)


<a id="org4f91cee"></a>

# Usage

-   sftp-get Download current file
-   sftp-put Upload current file
-   sftp-get-directory Download the entire folder
-   sftp-put-directory Upload the entire folder

