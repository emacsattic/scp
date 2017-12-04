[![MELPA](https://melpa.org/packages/scp-badge.svg)](https://melpa.org/#/scp)

[Readme here in English](README-en.md)  

# emacs-scp<a id="sec-1" name="sec-1"></a>

是使用SCP命令将文件传输到远程服务器  

# 安装<a id="sec-2" name="sec-2"></a>

推荐通过package.el安装,你可以使用以下命令:

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `scp` <kbd>[RET]</kbd>

## linux 环境依赖<a id="sec-2-1" name="sec-2-1"></a>

-   scp
-   sshpass

``` shell
    apt-get install sshpass
```

## windows 环境依赖<a id="sec-2-2" name="sec-2-2"></a>

-   pscp

	[下载pscp并将其加入到PATH中](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html)  

# 配置文件<a id="sec-3" name="sec-3"></a>

在当前项目下的.dir-locals.el中设置远程连接信息  

``` emacs-lisp
    ((nil
      (host ."127.0.0.1")
      (user ."username")
      (password ."password")
      (remote-path ."/www") ;;远程项目路径
      (port ."22")
    ))

```
需要读取本地变量，建议在引入scp.el之前加入  

``` emacs-lisp
    (setq enable-local-variables :all enable-local-eval t)
```
# 用法<a id="sec-4" name="sec-4"></a>

-   `scp-get` 下载当前文件
-   `scp-put` 上传当前文件
-   `scp-get-directory` 下载目录文件
-   `scp-put-directory` 上传目录文件
