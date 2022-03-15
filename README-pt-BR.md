Hematita da Lua
================
![](https://img.shields.io/crates/d/hematitia?style=for-the-badge) ![](https://img.shields.io/tokei/lines/github/danii/hematita?style=for-the-badge) ![](https://img.shields.io/crates/v/hematita?style=for-the-badge) ![](https://img.shields.io/badge/compiler%20version-1.53.0-007EC6?style=for-the-badge)
<br>
[![](https://img.shields.io/badge/crates.io-E6B14C?style=for-the-badge&logo=rust&logoColor=000000)](https://crates.io/crates/hematita) [![](https://img.shields.io/badge/lib.rs-282A36?style=for-the-badge&logo=rust)](https://lib.rs/crates/hematita) [![](https://img.shields.io/badge/github.com-24292E?style=for-the-badge&logo=github)](https://github.com/danii/hematita) [![](https://img.shields.io/badge/sponsor_me-FF69B4?style=for-the-badge&logo=github%20sponsors&logoColor=FFFFFF)](https://github.com/sponsors/danii) [![](https://img.shields.io/badge/telegram_group-26A5E4?style=for-the-badge&logo=telegram)](https://t.me/danii_hangout)

Hematita da Lua é um interpretador para a linguagem de script Lua, escrito inteiramente em Rust 100% seguro. Hematita é a palavra portuguesa para hematita, um tipo de óxido de ferro, ou ferrugem, e lua é a palavra portuguesa para lua. 'Hematita Da Lua' é um trocadilho com o projeto e a descoberta de que [o ferro na lua está inferrujando](https://www.nasa.gov/feature/jpl/the-moon-is-rusting-and-researchers-want-to-know-why).

O objetivo do projeto é fornecer um interpretador Lua reforçado e resiliente a vulnerabilidades de segurança. Ele faz isso usando nenhum código inseguro, sendo compilável em stable e contando com um número mínimo de dependências. Com isso, podemos ter certeza de que estamos protegidos contra quaisquer vulnerabilidades de segurança ainda não encontradas no código C. Sem desrespeito à implementação padrão de Lua e outros projetos C.

Dito isto, é importante notar que *Hematita não é estável*, está *muito cedo em seu desenvolvimento* e *pode estar com bugs*. Espero que, com tempo suficiente para amadurecer, Hematita consiga garantir essas coisas.

Executando Hematita
---------------------------
Se você quiser dar um test drive ao interpretador, execute `cargo install hematita_cli`, e então execute `hematita_cli`. Você será colocado em um REPL básico e poderá pressionar `Ctrl` + `C` a qualquer momento para sair. Uma grande proporção do código Lua que você joga nele deve funcionar bem, mas nem tudo, como alguns recursos de loops `for`. Por favor, registre um problema se encontrar algo que não funcione, e isso será corrigido em breve em uma versão futura!

A interface de linha de comando tem algumas opções; executá-lo com `--help` mostrará todos eles.
```
OPÇÕES:
-h, --help Exibe isso (comando de help) e sai
-V, --version Exibe informações de versão
-v, --verbose Executa com saída detalhada
-i, --interactive Executa no modo interativo, após executar SOURCE
-e, --evaluate Trata a fonte como código-fonte direto, em vez de um arquivo
-b, --byte-code Mostra o código de byte em vez de executar
-s, --ast Mostra a árvore de sintaxe abstrata em vez de executar
-t, --tokens Mostra tokens em vez de executar
```

Atualmente, `--verbose` não faz nada e é ignorado. Executar com `--help` ou `--version` impedirá que qualquer código seja executado. `--interactive` pode ser passado com um arquivo, e após a execução do arquivo terminar, você será colocado em um REPL com todo o estado do script deixado para você mexer. Usar `--evaluate` avaliará o código passado diretamente na linha de comando, em vez de carregá-lo de um arquivo.

As opções `--byte-code`, `--ast` e `--tokens` alteram a saída do interpretador. Usar `--byte-code` imprimirá o código de bytes compilado do programa em vez de executá-lo. `--ast` imprimirá a árvore de sintaxe abstrata interpretada em vez de executar, e `--tokens` imprimirá visualizações de depuração do fluxo de token em vez de executar. Cada uma dessas opções corresponde a um segmento diferente do interpretador, consulte a seção [internos] para obter mais informações.

Incorporando Hematita
------------------
A incorporação do Hematita é bastante simples e requer apenas o encadeamento de cada segmento do interpretador. Como sempre, requeira o engradado em seu `Cargo.toml`. Então, você está a apenas seis linhas de código de executar o código Lua em seu próximo grande projeto.
```rust
use hematita::{ast::{lexer, parser}, compiler, vm, lua_lib, lua_tuple};

// Lê o código lua
let source = "print(\"Hello, World!\")";
// Cria um lexer (apenas um token iterator) vindo dos caractéres do código fonte 
let lexer = lexer::Lexer {source: source.chars().peekable()}.peekable();
// Analisar do lexer um bloco de instruções
let parsed = parser::parse_block(&mut parser::TokenIterator(lexer)).unwrap();
// Compila o byteblock
let compiled = compiler::compile_block(&parsed);

// Prepara o global
let global = lua_lib::standard_globals();
// Criando a máquina virtual
let virtual_machine = vm::VirtualMachine::new(global);
// E roda o código byte
virtual_machine.execute(&compiled.into(), lua_tuple![].arc()).unwrap();
```
`VirtualMachine` é `Send` + `Sync`, e inclui um `'n` vitalício para as funções nativas e dados do usuário que você cria para sua conveniência. Então, elimine o velho `crossbeam::thread::Scope`, e enlouqueça. Se você estiver curioso sobre o que cada uma das linhas faz, consulte a seção [internos] para obter mais informações.

Criar sua própria função nativa também é fácil. Tudo o que é necessário é modificar o escopo global com qualquer função antiga como tipo, ou seja, qualquer `Fn` dinamicamente despachável.
```rust
let number = Mutex::new(0);
// Rust é triste se referindo aos parâmetros de closure, então é preciso de um &_. :(
let counter = move |_, _: &_| {
	let mut lock = number.lock().unwrap();
	let old = *lock;
	*lock += 1;
	Ok(lua_tuple![old].arc())
};

let global = {
	let globals = standard_globals();

	let mut data = globals.data.lock().unwrap();
	data.insert(lua_value!("counter"), Value::NativeFunction(&counter));
	drop(data);

	globals
};
```
