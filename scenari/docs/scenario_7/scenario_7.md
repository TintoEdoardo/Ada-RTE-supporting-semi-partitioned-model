# Scenario 7

Torna all'[indice](../index.md)

Questo scenario, costruito a partire dallo [scenario 6](../scenario_6/scenario_6.md), riguarda il:
- rappresentare e associare ad un task non più un solo budget, ma due: quello *high critical* e quello *low critical*;
- adattare il monitoraggio dell'esaurimento questi budget a seconda delle modalità di esecuzione del core (`HIGH` e `LOW`).
  
Bisogna:
- [X] Modificare il descrittore di un task in maniera tale che comprenda i due budget *high critical* e *low critical*
```
Low_Critical_Budget  : Time_Span;
High_Critical_Budget : Time_Span;
```
- [X] Creare due nuovi tipi task per differenziare i task *HI-CRIT* da quelli *LO-CRIT*
```
--  Periodic LO-CRIT task
task type Low_Crit_First_CPU
   (Pri                 : System.Priority;
    Low_Critical_Budget : Natural;
    Is_Migrable         : Boolean;
    Workload            : Positive;
    Period              : Positive) with CPU => 1
is
    pragma Priority (Pri);
end Low_Crit_First_CPU;

--  Periodic HI-CRIT task
task type High_Crit_First_CPU
   (Pri                  : System.Priority;
    Low_Critical_Budget  : Natural;
    High_Critical_Budget : Natural;
    Workload             : Positive;
    Period               : Positive) with CPU => 1
is
    pragma Priority (Pri);
end High_First_CPU;

```

   Distinguere i tasks *HI-CRIT* da quelli *LO-CRIT* a livello di tipi è utile perchè, come si vede dalla specifica, presentano differenze sostanziali: quelli *LO-CRIT* hanno un solo budget e possono essere migrabili, mentre quelli *HI-CRIT* hanno due budget e non sono mai migrabili.

- [X] Se il core sta eseguendo in modalità `LOW`
  - [X] le azioni di reazione (`CPU_BE_Detected`) ad un `CPU_Budget_Exceeded`, per i tasks **sia** di tipo `Low_Crit_First_CPU` che `High_Crit_First_CPU`, avvengono all'esaurirsi del proprio `Low_Critical_Budget`;
    - [X] Se ad esaurire il proprio `Low_Critical_Budget` è un task `Low_Crit_First_CPU`:
      - [X] **L'esperimento non è valido**. Proprio per questo è necessario saper rilevare comunque questa eventualità.
    - [X] Se ad esaurire il proprio `Low_Critical_Budget` è un task `High_Crit_First_CPU`: 
      - [X] la modalità di esecuzione del core deve essere portata ad `HIGH`;
      - [X] i `Low_Crit_First_CPU` che **sono migrabili** devono essere inseriti nella `Discarded_Thread_Table`;
      - [X] per i tasks `High_Crit_First_CPU`, il monitoraggio del consumo del loro budget sarà relativo al loro `High_Critical_Budget` e **non più** al loro `Low_Critical_Budget`;
      - [X] per i tasks `Low_Crit_First_CPU`, il monitoraggio del consumo del loro budget rimane relativo all'unico che possiedono, ovvero il `Low_Critical_Budget`;
      - [X] i tasks che possono potenzialmente prendere il core (*ready* e *suspended*) sono **tutti non migrabili**. 
      - [X] i tasks nella `Discarded_Thread_Table` sono **tutti migrabili**.
  

- [X] Se il core sta eseguendo in modalità `HIGH`
  - [X] per i tasks `Low_Crit_First_CPU`, le azioni di reazione (`CPU_BE_Detected`) ad un `CPU_Budget_Exceeded` avvengono ad esaurirsi del proprio `Low_Critical_Budget` e sono le stesse relative alla modalità d'esecuzione `LOW` del core.
  - [X] per i tasks `High_Crit_First_CPU`, le azioni di reazione (`CPU_BE_Detected`) ad un `CPU_Budget_Exceeded` avvengono ad esaurirsi del proprio `High_Critical_Budget`:
    - [X] **L'esperimento non è valido**. Proprio per questo è necessario saper rilevare comunque questa eventualità.

## Una considerazione sul come realizzare la logica appena esposta

Realizzare la logica appena descritta potrebbe essere molto *error prone*, oltre che complesso da verificare. Nel runtime Ravenscar, il descrittore di un task contiene due campi relativi alla propria priorità: uno contenente la priorità assegnata staticamente, mentre l'altro contenete la priorità che quel task assume in un certo momento dell'esecuzione.
```
--  Priorità assegnata static-time
Base_Priority : Priority;

--  Priorità a run-time  
Active_Priority : Priority;
```
Inizialmente le due priorità sono uguali, ma appena il task *entra* in un oggetto protetto, la sua `Active_Priority` viene alzata al *ceiling* di quell'oggetto (*Priority Ceiling Protocol*), per poi essere ripristinata non appena esso smette di utilizzare l'oggetto protetto. Si potrebbe semplificare la logica di monitoraggio del budget proprio ispirandosi a questo.

1. Il descrittore del task contiene:

```
--  Static-time allocated budget
Low_Critical_Budget  : Time_Span;
High_Critical_Budget : Time_Span;

--  Run-time budget
Active_Budget : Time_Span;
```
2. inizialmente, l'`Active_Budget` è uguale al `Low_Critical_Budget`;
3. appena la modalità di esecuzione del core passa da `LOW` ad `HIGH`, allora **per ogni** task *HI-CRIT*, l'`Active_Budget` viene impostato al valore contenuto in `High_Critical_Budget`;
4. quando si ritorna alla modalità `LOW`, allora **per ogni** task *HI-CRIT*, l'`Active_Budget` viene ripristinato a `Low_Critical_Budget`.

La logica di monitoraggio del budget si semplificherebbe in quanto le variabili da controllare sarebbero solo una e non più due. Di fatto, si monitorerebbe solo l'esaurimento dell'`Active_Budget` (indipendentemente da modalità d'esecuzione del core e criticità del task) invece di monitorare o il `Low_Critical_Budget` o l'`High_Critical_Budget` a seconda della modalità di esecuzione del core e del livello di criticità del task. Non sarebbe nulla di diverso dal monitoraggio che avviene fino allo [scenario 6](../scenario_6/scenario_6.md).

Quello che si dovrebbe fare è assicurarsi di aggiornare correttamente l'`Active_Budget`, cosa più semplice da realizzare e verificare. Tale aggiornamento deve avvenire ad ogni cambio di modalità di esecuzione del core.
- Se `LOW => HIGH`:
  - iterare sulla lista dei tasks ready
    - se il task corrente è *HI-CRIT* => `Active_Budget = High_Critical_Budget`
  - iterare sulla lista dei tasks sospesi
    - se il task corrente è *HI-CRIT* => `Active_Budget = High_Critical_Budget`
- Se `HIGH => LOW`:
  - iterare sulla lista dei tasks ready
    - se il task corrente è *HI-CRIT* => `Active_Budget = Low_Critical_Budget`
  - iterare sulla lista dei tasks sospesi
    - se il task corrente è *HI-CRIT* => `Active_Budget = Low_Critical_Budget`

L'iterare sulle liste, però, è temporalmente costoso. Si potrebbe mitigare la cosa in questo modo:
- sia `n` il numero di task *HI-CRIT*;
- essendo `n` noto staticamente, si può mantenere un array che contiene i riferimenti ai soli task *HI-CRIT*;
- al cambio di modalità d'esecuzione del core, si itera solo su questo array.

Sperabilmente, in un sistema reale, il numero di tasks non dovrebbe essere alto. Gli esperimenti degli autori, infatti, considerano taskset di dimensione al più 35 con una percentuale di *HI-crit* tasks che varia dal 10% al al 90%.

Torna all'[indice](../index.md)