# projeto-damas
Temos um dockerfile que há uma imagem de linux já com haskell, para instalar faça esse comando. **Não é necessário instala-lo, é só uma comodidade**

```bash
docker build . -t haskell:damas
docker run -it haskell:damas /bin/sh
```

Para rodar o programa primeiro faça o build

```bash
cabal build
```

Após, rode o programa

```sh
cabal run
```