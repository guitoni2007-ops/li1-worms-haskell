# 🐛 Worms — Laboratórios de Informática I

![Linguagem](https://img.shields.io/badge/Linguagem-Haskell-5e5086?style=for-the-badge&logo=haskell)
![Interface](https://img.shields.io/badge/Gráficos-Gloss-lightgrey?style=for-the-badge)

## 📌 Visão Geral
Este projeto consiste no desenvolvimento de um jogo 2D inspirado no clássico **Worms**, totalmente programado no paradigma da **programação funcional**. O jogo foi o projeto final da unidade curricular de Laboratórios de Informática I (1º Ano, 1º Semestre) da Universidade do Minho.

A interface gráfica, animações e sistema de eventos foram construídos de raiz utilizando a biblioteca gráfica `Gloss`.

## 🎯 Funcionalidades
* **Motor de Jogo Funcional:** Lógica de física, colisões, gravidade e sistema de turnos implementados de forma puramente funcional.
* **Arsenal e Utilitários:** Diversas armas e mecânicas de jogo implementadas (Bazuca, Dinamites, Minas, Jetpacks, Escavadoras, etc.).
* **Equipas e Países:** Sistema de equipas personalizadas com bandeiras de vários países.
* **Interface Gráfica (Gloss):** Menus interativos, renderização dinâmica de mapas e ecrãs de vitória.

---

## 🚀 Como Executar o Jogo

### Pré-requisitos
Para compilar e correr o jogo, precisas de ter o **GHC** (Glasgow Haskell Compiler) e o gestor de pacotes **Cabal** instalados no teu sistema.

### Instalação e Execução
1. Clona o repositório para a tua máquina:
   ```bash
   git clone [https://github.com/guitoni2007-ops/li1-worms-haskell.git](https://github.com/guitoni2007-ops/li1-worms-haskell.git)
   cd li1-worms-haskell
   ```

2. Compila e executa o jogo diretamente através do Cabal:
   ```bash
   cabal run
   ```

---

## 🛠️ Desenvolvimento e Ferramentas Académicas

Durante o desenvolvimento do projeto, foram utilizadas várias ferramentas de análise e teste exigidas para a cadeira:

### 1. Interpretador (REPL)
Para abrir o interpretador interativo (`GHCi`) com o projeto carregado para testar funções individualmente:
```bash
cabal repl
```

### 2. Testes Unitários e Cobertura (Coverage)
O projeto inclui executáveis para testar cada fase do desenvolvimento. Para correr os testes de uma tarefa (ex: Tarefa 1):
```bash
cabal run t1-feedback
```
Para gerar o relatório de cobertura de código (*Code Coverage*):
```bash
cabal clean
cabal run --enable-coverage t1-feedback
./runcoverage.sh t1
```

### 3. Documentação
O código-fonte está documentado de acordo com a norma do Haskell. Para gerar as páginas HTML da documentação:
```bash
cabal haddock-project
```

### 4. Qualidade de Código
Para medir a complexidade ciclomática e estrutural do projeto utilizando o **Homplexity**:
```bash
homplexity-cli --format=HTML lib/ > homplexity.html
```
