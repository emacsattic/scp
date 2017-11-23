
# Table of Contents

1.  [emacs-sftp](#org960dd57)
2.  [Installing](#org6dcc154)
    1.  [Linux dependent environment](#org29d554e)
    2.  [Windows depends on the environment](#org7e6fb88)
3.  [Configuration of the sample](#org0e56f80)
    1.  [Connection information Settings](#org127b31e)
4.  [Usage](#org736a1c8)



<a id="org960dd57"></a>

# emacs-sftp

SFTP is the mechanism to use the SCP command to transport files to a remote server  


<a id="org6dcc154"></a>

# Installing

Introduce sftp.el into your configuration and add it to your configuration  

    (require 'sftp)


<a id="org29d554e"></a>

## Linux dependent environment

-   scp
-   sshpass

    apt-get install sshpass


<a id="org7e6fb88"></a>

## Windows depends on the environment

-   pscp

[Download the PSCP tool here and add it to path](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html)  


<a id="org0e56f80"></a>

# Configuration of the sample


<a id="org127b31e"></a>

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


<a id="org736a1c8"></a>

# Usage

-   sftp-get Download current file
-   sftp-put Upload current file
-   sftp-get-directory Download the entire folder
-   sftp-put-directory Upload the entire folder

