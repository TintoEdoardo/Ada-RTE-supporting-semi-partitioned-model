# Scenario 12

Torna all'[indice](../index.md).

In questo scenario, costruito a partire dallo [scenario 11](../scenario_11/scenario_11.md), ci si preoccupa di realizzare gli effettivi meccanismi di migrazione dei task. Gli autori hanno identificato varie casistiche, le quali vengono affrontate una alla volta e in maniera tale che siano, fra loro, le più separate possibili.

Sia $`S`$ la steady mode, ovvero lo stato di esecuzione "normale": tutti i core stanno eseguendo in modalità *LO-crit*, quindi tutti i task sono assegnati al loro core originale.

$`X_1 = LO_1 \cup HI_1 \cup MIG_1`$\
$`X_2 = LO_2 \cup HI_2 \cup MIG_2`$\
$`S = X_1 \cup X_2`$

I sottoscenari che seguono si basano:
- o sullo stato $`S = X_1 \cup X_2`$;
- o su uno stato derivato da $`S = X_1 \cup X_2`$ e generato da altri sottoscenari.

Prima di tutto, è necessario: 
- avere un modo per rappresentare a quale CPU un task è correntemente (a runtime) associato;
- avere un modo, per ogni task. di tracciare la loro migrazione e il loro "rientro" alla CPU di partenza.

Bisogna:
- [X] modificare il descrittore del task
```
--  Statically allocated
Base_CPU   : CPU;

--  Run-time
Active_CPU : CPU
```
- [X] all'inizio, i due campi dovranno essere uguali.

- [X] modificare `Task_Data_Log`
```
Task_Data_Log is
record
  --  BE is Budget_Exceeded
  Times_BE         : Natural      := 0;
  Times_Discarded  : Natural      := 0;
  Times_Migrated   : Natural      := 0;
  Times_Restored   : Natural      := 0;
  Locked_Time      : Time_Span    := 0;
  Last_Time_Locked : Time         := 0;
end record;
``` 

## Scenario 12.1

Questo sottoscenario si basa sullo stato $`S`$ e riguarda l'evento `LOW => HIGH` di una sola CPU. Ovvero:
- si forza un `CPU_Budget_Exceeded` su di una CPU;
- i task migrabili di quella CPU vengono spostati sull'altra.

Questo deve essere fatto sia nel caso che a forzare un `CPU_Budget_Exceeded` sia la CPU1, sia nel caso che lo sia dalla CPU2.

Bisogna:
- [X] far sì che una CPU, che chiamiamo CPU A, forzi un `CPU_Budget_Exceeded`;
  - [X] in questo sottoscenario, **solo ed esclusivamente una delle due CPU deve forzarlo**, quindi la CPU B **non** deve mai causare un  `CPU_Budget_Exceeded`;
- [ ] alla sua rilevazione:
  - [X] incrementare `Task_Data_Log.Times_BE`;
  - [X] alzare il livello di criticità del core in oggetto (`LOW => HIGH`);
  - [X] tracciare `LOW => HIGH` incrementando `CPU_Data_Log.Low_To_High`;
  - [X] i task migrabili nella coda dei **pronti** della CPU A devono essere inseriti nella coda dei **pronti** della CPU B
    - [X] `Active_CPU := Target_CPU;` 
    - servirà un lock per ogni coda. Serve definire un protocollo di acquisizione dei lock in maniera tale da assicurare l'assenza di deadlock;
  - [X] incrementare, per tutti questi task, `Task_Data_Log.Times_Migrated`; 
  - [X] i task migrabili nella coda dei **sospesi** della CPU A devono essere inseriti nella coda dei **sospesi** della CPU B
    - ha senso? si rifletta sul fatto che forse è meglio migrare un task quando questo effettivamente viene risvegliato. Migrare un task sospeso dalla coda dei sospesi di A in quella di B, per poi riportarlo in quella di A, sarebbe inutile e forse troppo costoso;
    - in tal caso, si potrebbe fare che:
      1. se un task viene risvegliato;
      2. è migrabile;
      3. è sulla sua `Base_CPU`, cioè `Base_CPU = Active_CPU`
      4. la sua `Base_CPU` sta eseguendo in ***HI-crit*** mode; 
   
        allora quel task deve essere inserito nella coda dei pronti dell'altro core;
    - [X] incrementare, per tutti questi task, `Task_Data_Log.Times_Migrated`;
- [X] verificare il tutto facendo sì che la CPU A sia la CPU**1**. Allora, a questo punto, lo stato dell'esecuzione sarà:\
$`Y(1)_1 = LO_1 \cup HI_1`$\
$`Y(1)_2 = LO_2 \cup HI_2 \cup MIG_2 \cup MIG_1`$\
$`S_1' = Y(1)_1 \cup Y(1)_2`$\
Si è quindi osservata la transazione $`S \rightarrow S_1'`$ 

- [X] verificare il tutto facendo sì che la CPU A sia la CPU**2**. Allora, a questo punto, lo stato dell'esecuzione sarà:\
$`Y(2)_1 = LO_1 \cup HI_1 \cup MIG_1 \cup MIG_2`$\
$`Y(2)_2 = LO_2 \cup HI_2`$\
$`S_2' = Y(2)_1 \cup Y(2)_2`$\
Si è quindi osservata la transazione $`S \rightarrow S_2'`$ 

## Scenario 12.2
- [X] alla rilevazione di un *idle tick* sulla CPU A
  - [X] i tasks con `Base_CPU = A` che sono nella coda dei pronti della CPU B devono essere spostati nella coda dei pronti della CPU A
    - [X] incrementare `Task_Data_Log.Times_Restored`
    - come comportarsi se uno dei tasks con `Base_CPU = A` sta eseguendo sulla CPU B? In teoria, questo task dovrebbe essere fermato per poi essere messo nella coda dei pronti della CPU A.
      - sarà sufficiente spostarlo da una coda all'altra. Questo perchè, nel runtime, al termina delle operazioni di modifica delle code, viene controllato se è necessario o meno un context switch. Un context switch è necessario se e solo se la testa della coda dei pronti è stata aggiornata, che è ciò che succede in questo caso specifico. 
  - [X] i tasks con `Base_CPU = A` che sono nella coda dei **sospesi** della CPU B devono essere spostati nella coda dei **pronti** della CPU A
    -  come prima: ha senso? forse è meglio ripristinare un task solo quando esso viene effettivamente svegliato.
    -  in tal caso, si potrebbe fare che:
       1. se un task viene risvegliato;
       2. è migrabile;
       3. **non** è sulla sua `Base_CPU`, cioè `Base_CPU /= Active_CPU`;
       4. la sua `Base_CPU` sta eseguendo in ***LO-crit*** mode;

        allora quel task deve essere inserito nella coda dei pronti della sua `Base_CPU`, ovvero essere ripristinato alla sua CPU originale;
    - [X] incrementare, per tutti questi task, `Task_Data_Log.Times_Migrated`; 

- [X] verificare il tutto facendo sì che la CPU A sia la CPU**1**. Allora, a questo punto, lo stato dell'esecuzione sarà:\
$`X_1 = LO_1 \cup HI_1 \cup MIG_1`$\
$`X_2 = LO_2 \cup HI_2 \cup MIG_2`$\
$`S = X_1 \cup X_2`$\
Si è quindi osservata la transazione $`S_1' \rightarrow S`$

- [X] verificare il tutto facendo sì che la CPU A sia la CPU**2**. Allora, a questo punto, lo stato dell'esecuzione sarà:\
$`X_1 = LO_1 \cup HI_1 \cup MIG_1`$\
$`X_2 = LO_2 \cup HI_2 \cup MIG_2`$\
$`S = X_1 \cup X_2`$\
Si è quindi osservata la transazione $`S_2' \rightarrow S`$

Vai al [prossimo scenario](../scenario_13/scenario_13.md).\
Torna all'[indice](../index.md).