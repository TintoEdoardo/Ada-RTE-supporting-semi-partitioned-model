# Scenario 6

Torna all'[indice](../index.md)

Questo scenario, costruito a partire dallo [scenario 5](../scenario_5/scenario_5.md), riguarda il ripristinare i tasks contenuti nella `Discarded_Thread_Table` al momento opportuno.
Secondo il modello di Xu & Burns, semplificato per questo scenario, questi tasks devono essere ripristinati quando il livello di criticità del core scenda da `HIGH` a `LOW`. Sempre secondo gli autori, questo deve avvenire alla rilevazione di un *idle tick*. Nel runtime Ravenscar, questo può essere corrisposto al momento nel quale il **task di idle**, cioè quello che esegue se nessun'altro task può eseguire, **prende possesso del core**.\
Negli scenari visti fino ad ora, il `main` (che è pur sempre un task), ha priorità minima (cioè zero) ed è sempre pronto ad eseguire, impedendo al task di idle di prendere possesso del core. Serve quindi far sì che anch'esso diventi un task periodico che compie del lavoro. In futuro, questo task potrebbe essere utile per stampare sulla porta seriale UART ciò che è stato tracciato nella esecuzione di un esperimento.

**D'ora in avanti**, per brevità, si usa la notazione `LOW => HIGH` per indicare il momento nel avviene il cambio di modalità di esecuzione di un core da `LOW` a `HIGH`. Viceversa, si usa `HIGH => LOW`.

- [ ] modificare il `main` in maniera tale che sia periodico e non forzi mai un `CPU_Budget_Exceeded`. Il `main` deve essere trattato come un task che non può **mai** migrare. Per ora, può essere trattato come se fosse un task *HI-CRIT*.
- [ ] Utilizzando i task set 1 e 2 dello [scenario 5](../scenario_5/scenario_5.md), verificare che:
  - [ ] accada quello che è stato descritto nello [scenario 5](../scenario_5/scenario_5.md)
  - [ ] all'esecuzione del task di idle, se il core sta eseguendo in modalità `HIGH`, allora:
    - [ ] la modalità d'esecuzione deve essere portata a `LOW`
    - [ ] i task nella `Discarded_Thread_Table` devono essere ripristinati
      - [ ] cosa vuol dire precisamente? Che devono essere rimessi nella coda di pronti? Che devono essere sospesi e rilasciati al loro prossimo rilascio? Penso che la seconda opzione sia la più corretta: rimetterli in coda di pronti potrebbe portare a far sì che tali task eseguano **oltre** il loro prossimo punto di rilascio. È più sicuro (e trattabile) far si che riprendano l'esecuzione al primo punto di rilascio successivo a `HIGH => LOW`. Il prossimo punto di rilascio deve però essere computato. Per ogni task che è stato nella `Discarded_Thread_Table`, bisognerebbe capire quanti rilasci sarebbero stati effettutati dall'inizio dell'esperimento fino al momento di ripristino (`HIGH => LOW`).\
      Supponiamo che quest'ultima quantità temporale sia pari a `k` e che il periodo di un certo task `T` (privo di offset) nella `Discarded_Thread_Table` sia pari a `p`. Questo vuol dire che, senza cambio di criticità di esecuzione del core `LOW => HIGH`, sarebbero occorsi `n = floor (k/p)` rilasci. Supponendo che `t` sia l'istante temporale assoluto corrispondente allo zero logico di inizio dell'esperimento, allora il prossimo rilascio di `T` successivo al cambio di criticità `HIGH => LOW` sarà all'istante temporale assoluto `t + (n+1)*p`.


Torna all'[indice](../index.md)