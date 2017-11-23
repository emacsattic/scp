
<a id="orgc869146"></a>

# emacs-sftp

SFTP is the mechanism to use the SCP command to transport files to a remote server  


<a id="orgd6aac41"></a>

# Installing

Introduce sftp.el into your configuration and add it to your configuration  

    (require 'sftp)


<a id="orgb3d0a81"></a>

## Linux dependent environment

-   scp
-   sshpass

    apt-get install sshpass


<a id="orgf49562e"></a>

## Windows depends on the environment

-   pscp

[Download the PSCP tool here and add it to path](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html)  


<a id="org10547b7"></a>

# Configuration of the sample


<a id="org3cfd371"></a>

## Connection information Settings

Use .dir-locals.el to set the connection information for the current project  

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


<a id="org10108ff"></a>

# Usage

-   sftp-get Download current file
-   sftp-put Upload current file
-   sftp-get-directory Download the entire folder
-   sftp-put-directory Upload the entire folder

