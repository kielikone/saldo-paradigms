all: python

lib:
	cd saldo-inflector; make

python: lib
	cp -r saldo-inflector/lib saldo-python/src/saldo/lib
	cp README.md saldo-python/README.md
	cd saldo-python; \
	  python3 -m build; \
	  python3 -m wheel tags --platform-tag $(shell python3 saldo-python/correct_tag.py) dist/saldo_python-*-py3-none-any.whl; \
	  rm dist/saldo_python-*-py3-none-any.whl dist/saldo-python-*.tar.gz

docker-python-linux-amd64:
	docker build . --platform=linux/amd64 -t saldobuilder-linux-amd64
	docker run -v $(PWD)/saldo-python/dist:/home/nonroot/saldo-python/dist -it saldobuilder-linux-amd64

docker-python-linux-aarch64:
	docker build . --platform=linux/aarch64 -t saldobuilder-linux-aarch64
	docker run -v $(PWD)/saldo-python/dist:/home/nonroot/saldo-python/dist -it saldobuilder-linux-aarch64
	
clean:
	cd saldo-inflector; make clean
	rm -rf saldo-python/src/saldo/lib
	rm -f saldo-python/README.md
	rm -rf saldo-python/dist

.PHONY: all clean
