# Scenario 2

Questo scenario, costruito a partire dallo [scenario 1](../scenario_1/scenario_1.html), riguarda il rappresentare e il rilevare l'**overrun** di un task.
L'overrun di un task si verifica quando esso esegue sulla CPU per oltre una quantità temporale prestabilita (*budget*).

Bisogna:

- [ ] rappresentare il budget di un task. 
  - [ ] deve essere espresso in microsecondi.
  - [ ] deve essere compreso nella specifica di un task.
  ```
     task type Periodic_First_CPU
     (Pri    : System.Priority;
      Budget : Ada.Real_Time.Microseconds
      Period : Positive) with CPU => 1
    is
      pragma Priority (Pri);
   end Periodic_First_CPU;
  ```
  - [ ] un budget pari a $0$ (zero) rappresenta un task il quale tempo di esecuzione non è soggetto a monitoraggio.
- [ ] impostarlo prima dell'esecuzione del corpo del task, ovvero all'interno della sua *regione dichiarativa*.
  - questo implica che il descrittore di quel task dovrà contenere un campo relativo al suo budget. 
- [ ] forzare il task ad eseguire per oltre il suo budget, così da ricreare un overrun.
- [ ] al verificarsi dell'overrun deve essere invocato un apposito gestore (*handler* `Overrun_Detected`) che scrive sulla porta seriale (UART) "Overrun detected". Tale handler **deve** eseguire sullo stesso core sul quale l'overrun è stato osservato (per ora il core0). Questo implica che l'handler deve prendere il possesso del core a discapito del task che ha ecceduto il suo budget. Siano:
  -   `t`, l'istante *assoluto* nel tempo nel quale il task `P1` (ri-)prende possesso del core
  -   `x`, la quantità temporale corrispondente al budget di P1
  -   allora, nell'istante `t`:
      - [ ] deve essere armato un evento temporale al punto temporale *assoluto* `t+x` non appena `P1` prende possesso del core.
        -  al netto di problemi di ordine di elaborazione, il package `Ada.Real_Time.Timing_Events` è utile in tal senso.
      - [ ] se il task perde il possesso del core a causa di
        -  ~~*preemption* da parte di un task *ready* a priorità più alta~~. Non può accadere in questo scenario in quanto esiste solo il task `P1`.
        -  esecuzione dell'istruzione di `delay_until`
        
        allora l'evento temporale deve essere **disarmato**. In generale, il disarmo deve avvenire ogni volta che il task perde/cede il core.

      - [ ] se nel punto temporale assoluto `t+x` il task è ancora in esecuzione sul core, allora vuol dire che che il task sta eseguendo oltre il suo budget => **overrun**. Nella pratica, visto quanto detto al punto precedente, l'overrun si verifica **solo ed esclusivamente** se il disarmo dell'evento temporale non avviene in tempo o non correttamente. "Dimenticare" un disarmo causerebbe la rilevazione di un overrun fasullo, cosa che chiaramente non deve accadere.  Il verificarsi di tale evento temporale corrisponde all'esecuzione dell'handler `Overrun_Detected`
