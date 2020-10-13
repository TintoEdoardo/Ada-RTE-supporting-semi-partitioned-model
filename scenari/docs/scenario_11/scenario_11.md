# Scenario 11

Torna all'[indice](../index.md).

In questo scenario, costruito a partire dallo [scenario 10](../scenario_10/scenario_10.md), ci si occupa, per ambo i core, di:
- reagire, in maniera preliminare, alla rilevazione di un `CPU_Budget_Exceeded`;
- tracciare, per ogni task:
  -  quante volte incorre in un `CPU_Budget_Exceeded`;
  -  quante volte viene inserito ed estratto nella `Discarded_Thread_Table`;
     - che è **unica** per tutti i core;
- tracciare, per ogni core:
  - quanti cambi `LOW => HIGH` e `HIGH => LOW` sono avvenuti;
  - per quanto tempo, durante l'esecuzione del taskset, il core rimane *idle*;
  - per quanto tempo, durante l'esecuzione del taskset, il core rimane bloccato in attesa di ottenere un lock;
    - in futuro, una certa CPU dovrà modificare le code dell'altra CPU. Per evitare problemi di *race condition*, serve sincronizzazione tramite lock. Non è possibile, credo, dire a priori se l'overhead derivato sia trascurabile o meno. Ergo, serve misurarlo. 
- fermare, dopo una certa quantità temporale `t` prefissata, l'esecuzione del tasket (cioè dei core) per stampare sulla porta seriale quanto tracciato;
  - `t` deve essere sufficientemente grande da permettere il tracciamento di più cambi di criticalità.

Bisogna:
- [X] creare, per ogni task, una apposita struttura dati per il tracciamento;
```
Task_Data_Log is
record
  --  BE is Budget_Exceeded
  Times_BE         : Natural      := 0;
  Times_Discarded  : Natural      := 0;
  Times_Restored   : Natural      := 0;
  Locked_Time      : Time_Span    := 0;
  Last_Time_Locked : Time         := 0;
end record;
``` 
- [X] creare, per ogni core, una apposita struttura dati per il tracciamento;
```
CPU_Data_Log is
record
  Low_To_High        : Natural      := 0;
  High_To_Low        : Natural      := 0;

  Idle_Time          : Time_Span    := 0;
  Last_Time_Idle     : Time         := 0;
end record;
``` 
- [X] alla rilevazione di un `CPU_Budget_Exceeded`
  - [X] incrementare `Task_Data_Log.Times_BE`;
  - [X] alzare il livello di criticità del core in oggetto (`LOW => HIGH`);
  - [X] tracciare `LOW => HIGH` incrementando `CPU_Data_Log.Low_To_High`; 
  - [X] spostare i task migrabili dalla coda dei pronti di quel core alla `Discarded_Thread_Table`;
  - [X] per ognugno di essi, incrementare `Task_Data_Log.Times_Discarded`;
  - [X] per i task *HI-crit* di quel core, il loro *budget monitoring* deve essere relativo al proprio budget ***HI**-crit*;
- [X] non appena su quel core esegue il task di idle: 
  - [X] abbassare il livello di criticità del core in oggetto (`HIGH => LOW`);
  - [X] tracciare `HIGH => LOW` incrementando `CPU_Data_Log.High_To_Low`;
  - [X] (ri-)spostare i task migrabili dalla `Discarded_Thread_Table` alla coda dei pronti di quel core. Il calcolo del prossimo release dei job di questi task è lo stesso dello [scenario 6](../scenario_6/scenario_6.md);
  - [X] per ognuno di essi, incrementare `Task_Data_Log.Times_Restored`
  - [X] per i task *HI-crit* di quel core, il loro *budget monitoring* deve essere relativo al proprio budget ***LO**-crit*;
  - [X] iniziare a tenere traccia del tempo che passerà fra questo momento e quando un task vuole (ri-)prende possesso della CPU.
    - `CPU_Data_Log.Last_Time_Idle := Read_Clock;` 
- [X] appena un task (ri-)prende possesso della CPU
  - [X] se prima di esso stava eseguendo il task di idle => aggiornare `Idle_Time := Idle_Time + (Read_Clock - Last_Time_Idle);`
- [X] se, nel tentare di prendere un lock, il task rimane bloccato => `Task_Data_Log.Last_Time_Locked := Read_Clock;`
- [X] una volta che riesce a prenderlo, aggiornare `Locked_Time := Locked_Time + (Read_Clock - Last_Time_Locked);`
- [X] una volta che la quantità temporale `t` prefissata è trascorsa, l'esecuzione del taskset deve essere fermata;
  - [X] per ogni core, un task a priorità massima ne prende il possesso senza mai perderlo;
  - [X] solo ed esclusivamente il task sul primo core deve stampare sulla porta seriale UART tutto ciò che è stato tracciato. 

Vai al [prossimo scenario](../scenario_12/scenario_12.md).\
Torna all'[indice](../index.md).