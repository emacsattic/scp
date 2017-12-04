
# emacs-scp<a id="sec-1" name="sec-1"></a> [![MELPA](https://melpa.org/packages/scp-badge.svg)](https://melpa.org/#/scp)

Scp is the mechanism to use the SCP command to transport files to a remote server  

# Installing<a id="sec-2" name="sec-2"></a>

Recommended through the package.el installation, you can use the following command :

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `scp` <kbd>[RET]</kbd>

## Linux dependent environment<a id="sec-2-1" name="sec-2-1"></a>

-   scp
-   sshpass

``` shell
    apt-get install sshpass
```

## Windows depends on the environment<a id="sec-2-2" name="sec-2-2"></a>

-   pscp

	[Download the PSCP tool here and add it to path](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html)  

# Configuration of the sample<a id="sec-3" name="sec-3"></a>

## Connection information Settings<a id="sec-3-1" name="sec-3-1"></a>

Use .dir-locals.el to set the connection information for the current project  

``` emacs-lisp
    ((nil
      (host ."127.0.0.1")
      (user ."username")
      (password ."password")
      (remote-path ."/www") ;;Remote server engineering path 
      (port ."22")
    ))
```

Therefore, you need to read the variables and suggest joining before you introduce scp.el  

``` emacs-lisp
    (setq enable-local-variables :all enable-local-eval t)
```

# Usage<a id="sec-4" name="sec-4"></a>

-   `scp-get` Download current file
-   `scp-put` Upload current file
-   `scp-get-directory` Download the entire folder
-   `scp-put-directory` Upload the entire folder
