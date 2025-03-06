# projeto-damas
Temos um dockerfile que há uma imagem de linux já com haskell, para instalar faça esse comando. **Não é necessário instala-lo, é só uma comodidade**

```bash
docker build . -t haskell:damas
docker run -it haskell:damas /bin/sh
```

Para rodar o programa primeiro faça o download das bibliotecas

```bash
cabal install
```

Após, faça o build e rode o programa

```sh
cabal build
cabal run
```
