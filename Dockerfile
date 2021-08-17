FROM opencpu/base
RUN R -e 'remotes::install_github("resplab/codexcopd")'
RUN R -e 'remotes::install_github("resplab/codexcopdPrism")'
RUN echo "opencpu:opencpu" | chpasswd
