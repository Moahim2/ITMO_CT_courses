FROM dokken/ubuntu-20.04:latest

LABEL maintainer="renbou"

# Install all required tools
RUN apt update && \
    apt install -y clang-tidy-10 clang-11 clang-format cmake && \
    apt install -y pkg-config &&\
    apt install -y libjemalloc-dev

# Make clang the default compiler, make clang-tidy link
RUN update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-11 60 && \
    update-alternatives --install /usr/bin/cc cc /usr/bin/clang-11 60 && \
    update-alternatives --install /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-10 100

RUN mkdir /env
WORKDIR /env

COPY app/libica-plugin.so .
COPY app/app.sh .

ENV REPO_PATH {{container_repo_path}}

ENTRYPOINT ["bash", "app.sh"]
