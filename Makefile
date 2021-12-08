# Copyright 2019-present Cornell University
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

NAME=p4pp

.PHONY: all build clean %.exe

all: build

build:
	dune build @install

doc:
	dune build @doc

run:
	dune exec $(NAME)

install:
	dune install

clean:
	dune clean

test:
	dune exec $(NAME) ./ebpf_model.p4
