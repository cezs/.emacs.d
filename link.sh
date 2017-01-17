#!/bin/sh

dotfiles=$(pwd)/*

cd

for path in $dotfiles
do
    filename="$(basename $path)"
    if [[("$filename" != "link.sh") && ("$filename" != "link.sh~")]]
    then
        if [ ! -h ~/.emacs.d/$filename ]
        then
            echo "Creating symlink for $filename..."
            ln -s $path ~/.emacs.d/$filename
        else
            echo "Creating symlink and backing-up $filename..."
            ln -s -b $path ~/.emacs.d/$filename
        fi
    fi
    echo $filename
done

echo "Finished creating symlinks."
