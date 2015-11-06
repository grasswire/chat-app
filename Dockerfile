FROM ubuntu:14.04
ADD . /code
WORKDIR /code
RUN sudo apt-get update -y
RUN sudo apt-get install -y wget
RUN sudo apt-get install -y build-essential zlib1g-dev
RUN sudo apt-get install -y libpq-dev
RUN wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
RUN echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
RUN sudo apt-get update && sudo apt-get install stack -y
RUN stack install yesod-bin cabal-install --install-ghc
RUN stack build
