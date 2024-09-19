# Use Ubuntu as the base image
FROM ubuntu:22.04

# Avoid prompts from apt
ENV DEBIAN_FRONTEND=noninteractive

# Install necessary packages
RUN apt-get update && apt-get install -y \
    curl \
    git \
    z3 \
    build-essential \
    libffi-dev \
    libffi8ubuntu1 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Install GHCup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh


# Add GHCup to PATH
ENV PATH="/root/.ghcup/bin:${PATH}"

# Install specific versions of GHC, cabal, and Stack
RUN ghcup install ghc 9.2.7 \
    && ghcup install cabal 3.10.1.0 \
    && ghcup install stack 2.9.3

# Set GHC 9.2.7 as the default version
RUN ghcup set ghc 9.2.7

# Clone the hani repository
RUN git clone https://github.com/VMCAI25-Hani/hani.git /hani

# Set working directory
WORKDIR /hani

# Build the project
RUN stack build

# Set the default command
CMD ["/bin/bash", "-c", "stack run +RTS -N -- parallel experiment1 && stack run +RTS -N -- coverage experiment1"]