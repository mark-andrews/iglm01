FROM xmjandrews/verse:20.07.15

RUN install2.r --error here \
                       pander \
                       pscl \
                       nnet

RUN rm -rf /tmp/downloaded_packages /tmp/*.rds

RUN sudo apt-get update && \
	sudo apt-get install -y --no-install-recommends \
	texlive-fonts-recommended \
  texlive-fonts-extra \
  texlive-bibtex-extra \
  texlive-plain-generic