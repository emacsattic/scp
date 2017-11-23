
[Readme here in English](README-en.md)  


<a id="orgf83b7af"></a>

# emacs-sftp

是使用SCP命令将文件传输到远程服务器  


<a id="orgec14bd4"></a>

# 安装

在配置文件中引入sftp.el  

    (require 'sftp)


<a id="orgb171ddc"></a>

## linux 环境依赖

-   scp
-   sshpass

    apt-get install sshpass


<a id="orgfe25dac"></a>

## windows 环境依赖

-   pscp

[下载pscp并将其加入到PATH中](https://www.chiark.greenend.org.uk/~sgtatham/putty/latest.html)  


<a id="orgb3a25c1"></a>

# 配置文件

在当前项目下的.dir-locals.el中设置远程连接信息  

    ((nil
      (host ."127.0.0.1")
      (user ."username")
      (password ."password")
      (remote_path ."/www") ;;远程项目路径
      (port ."22")
    ))

需要读取本地变量，建议在引入sftp之前加入  

    (setq enable-local-variables :all enable-local-eval t)
      (hack-dir-local-variables)


<a id="org30db78a"></a>

# 用法

-   sftp-get 下载当前文件
-   sftp-put 上传当前文件
-   sftp-get-directory 下载目录文件
-   sftp-put-directory 上传目录文件

