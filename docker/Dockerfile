# (C) 2016 David Lettier
# http://www.lettier.com/
# https://github.com/lettier/gifcurry

FROM haskell:8
RUN echo "deb http://ftp.uk.debian.org/debian jessie-backports main" >> /etc/apt/sources.list
RUN apt-get update
RUN yes | apt-get install build-essential
RUN yes | apt-get install xdg-utils
RUN yes | apt-get install exo-utils
RUN yes | apt-get install viewnior
RUN yes | apt-get install pkg-config
RUN yes | apt-get install glade
RUN yes | apt-get install ffmpeg
RUN yes | apt-get install imagemagick
RUN mkdir -p /root/.local/share/applications/
RUN xdg-mime default viewnior.desktop image/gif
RUN cabal sandbox init
RUN cabal update
RUN cabal install alex happy -j
RUN cabal install gtk2hs-buildtools -j
RUN cabal install gifcurry -j
RUN ln -s /.cabal-sandbox/bin/gifcurry_cli /usr/local/bin/gifcurry_cli
RUN ln -s /.cabal-sandbox/bin/gifcurry_gui /usr/local/bin/gifcurry_gui
ENTRYPOINT ["bash"]
