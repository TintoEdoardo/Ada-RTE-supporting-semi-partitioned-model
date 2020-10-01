# Scenario 3

Torna all'[indice](../index.md)

Questo scenario, costruito a partire dallo [scenario 2](../scenario_2/scenario_2.md), si occupa di:
- aggiungere una casistica nella quale si deve disarmare l'evento temporale associato al task che sta perdendo/cedendo il core.
- formulare una prima (ma non completa) serie di azioni da effettuare in caso di rilevazione di un `CPU_Budget_Exceeded`.

## Disarmo evento
Un task `P1` in esecuzione può:
1. cedere il core sul quale è in esecuzione in quanto sta eseguendo l'istruzione `delay_until X` dove `X` è un istante temporale **assoluto** strettamente maggiore di quello corrente;
2. perdere il core subendo preemption da un task `P2` a priorità strettamente più alta, ovvero il task `P1` in esecuzione non è più in testa alla coda di pronti di quel core.
3. ~~perdere il core a causa dell'arrivo di un *interrupt*.~~ Nelle nostre sperimentazioni, l'unico interrupt che può fare preemption su di un task in esecuzione è quello legato al suo eventuale `CPU_Budget_Exceeded`. Questo implica che un evento temporale precedentemente armato è scaduto => non esiste più nessun evento temporale da disarmare.

Il punto $1$ è oggetto dello [scenario 2](../scenario_2/scenario_2.md).
Di seguito, si tratterà il punto $2$.
- [X] serve un `type task` che rappresenti quei task che non sono soggetti a monitoraggio del tempo di esecuzione.
  ```
  --  Periodic task that is not subject to budget monitoring
  task type Periodic_Not_Monitored
   (Pri      : System.Priority;
    Period   : Positive) with CPU => 1
  is
    pragma Priority (Pri);
  end Periodic_Not_Monitored;
  ``` 
- [X] per questo scenario, un task di tipo `Periodic_Not_Monitored` non deve occupare per troppo tempo la CPU. Il suo corpo sarà come quello di tasks di tipo `Periodic_First_CPU` definito nello **[scenario 0](../scenario_0/scenario_0.md)**
- [X] far sì che tasks di questo tipo non siano effettivamente monitorati in questo senso. Questo vuol dire che quando questi tasks eseguono, **non esiste** alcun evento temporale, per quel core, che sia armato.
  - [X] ogni qual volta che un task di tipo `Periodic_Not_Monitored` prende il possesso del core, tutti gli eventi temporali (ce ne sarà solo uno in realtà) armati su quest'ultimo devono essere **disarmati**. 
- [X] istanziare un task `PNM_1` di tipo `Periodic_Not_Monitored` (oltre a `P1`) tale che $Priority(PNM_1) > Priority(P1)$

```
  P1    : Periodic_First_CPU (Pri => 10, Budget => 200_000, Period => 400_000);
  PNM_1 : Periodic_Not_Monitored (Pri => 11, Period => 1_200_000);
```
- [X] il corpo di `P1` non dovrà mai causare effettivamente un `CPU_Budget_Exceeded`, quindi il suo tempo di esecuzione deve essere al di sotto di `200_000` microsecondi.
- [X] almeno un rilascio di `PNM_1` deve avvenire nel mentre che `P1` sta eseguendo. In seguito:
  - [X] l'evento temporale legato all'eventuale `CPU_Budget_Exceeded` di `P1` deve essere **disarmato**
  - [X] context switch da `P1` a `PNM_1`;
  - [X] nessun evento temporale relativo a `PNM_1` deve essere armato, in quanto il suo budget è $0$. 
  - [X] al termine dell'esecuzione di `PNM_1`, `P1` riprende l'esecuzione => context switch da `PNM_1` a `P1`.
  - [X] al termine del context switch, un nuovo evento temporale, relativo all'eventuale `CPU_Budget_Exceeded` di `P1`, deve essere armato con scadenza all'istante temporale assoluto `Now + Budget`.

## Una preliminare serie di azioni in risposta ad un `CPU_Budget_Exceeded`
In questo sotto-scenario, i task `P1` e `PNM_1` **non** devono essere istanziati.
- [X] creare un nuovo *type task* le quali istanze saranno relative a task che, prima o poi, forzeranno un `CPU_Budget_Exceeded`.
```
  task type BE_First_CPU
    (Pri     : System.Priority;
    Budget   : Natural;
    Workload : Positive;
    Period   : Positive) with CPU => 1
  is
    pragma Priority (Pri);
  end BE_First_CPU;
``` 
- [X] il corpo sarà:
```
task body BE_First_CPU is
    Next_Period : Ada.Real_Time.Time;
    Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
    I : Natural := 1;
begin
    Next_Period := Ada.Real_Time.Clock + Period_To_Add;
    loop
      delay until Next_Period;

      if I mod 3 = 0 then
        Production_Workload.Small_Whetstone (Workload);
      end if;

      I := I + 1;

      Next_Period := Next_Period + Period_To_Add;
    end loop;
end BE_First_CPU;
```
- [ ] istanziare un task `BE1` di tipo `BE_First_CPU`. Il valore del parametro `Workload` deve essere modulato in maniera tale da eccedere il budget allocato per `BE1`.
- [X] il `CPU_Budget_Exceeded` di `BE1` deve essere forzato ogni 3 iterazioni.
  - non c'è un motivo sulla scelta di questo numero e, per ora, non è di interesse.
- [X] al verificarsi del `CPU_Budget_Exceeded`:
  - [X] `BE1` deve essere forzatamente rimosso dal core, ovvero essere rimosso dalla coda dei pronti di quel core.
  - [X] essere inserito in una apposita coda (chiamata `Discarded_Thread_Table`) che contiene i task che sono stati forzatamente rimossi da quello specifico core. 
    - per ora, un task inserito in questa coda, non viene mai più re-inserito nella coda dei pronti.
    - la coda dei rimossi deve essere strutturalmente il più simile possibile a quella dei pronti e dei sospesi. In questo modo, sarà più probabile riuscire a riutilizzare le operazioni sulle code che sono già presenti nel runtime Ravenscar.

Torna all'[indice](../index.md)