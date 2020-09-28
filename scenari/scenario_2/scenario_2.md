# Scenario 2

Questo scenario, costruito a partire dallo [scenario 1](../scenario_1/scenario_1.md), riguarda il rappresentare il budget di un task e rilevare se il suo tempo di esecuzione sulla CPU **eccede** questo questo budget. Nominiamo quest'ultima eventualità come `CPU_Budget_Exceeded`.

Bisogna:

- [x] rappresentare il budget di un task. 
  - [X] l'applicazione deve poterlo esprimere in microsecondi.
  - [X] deve essere compreso nella specifica di un task.
  ```
    task type Periodic_First_CPU
     (Pri    : System.Priority;
      Budget : Ada.Real_Time.Microseconds
      Period : Positive) with CPU => 1
    is
      pragma Priority (Pri);
   end Periodic_First_CPU;
  ```
- [X] ~~impostarlo prima dell'esecuzione del corpo del task, ovvero all'interno della sua *regione dichiarativa*.~~ Il compilatore non permette di invocare procedure di questo tipo nella zona dichiarativa. Ergo, tutte le operazioni di questo tipo devono essere fatte appena dopo il `begin` del task. Servirà in futuro un meccanismo che forzi i task ad iniziare ad eseguire il loro `loop` nello stesso istante.
  - questo implica che il descrittore di quel task dovrà contenere un campo relativo al suo budget. 
- [X] forzare il task ad eseguire per oltre il suo budget, così da ricreare un `CPU_Budget_Exceeded`.
- [x] al verificarsi di un `CPU_Budget_Exceeded`, deve essere invocato un apposito gestore (*handler* `CPU_BE_Detected`) che scrive sulla porta seriale (UART) "CPU_Budget_Exceeded detected". Tale handler **deve** eseguire sullo stesso core sul quale il `CPU_Budget_Exceeded` è stato osservato (per ora il core0). Questo implica che l'handler deve prendere il possesso del core a discapito del task che ha ecceduto il suo budget. Siano:
  -   `t`, l'istante *assoluto* nel tempo nel quale il task `P1` (ri-)prende possesso del core
  -   `x`, la quantità temporale corrispondente al budget di P1
  -   allora, nell'istante `t`:
      - [x] deve essere armato un evento temporale al punto temporale *assoluto* `t+x` non appena `P1` prende possesso del core.
        -  al netto di problemi di ordine di elaborazione, il package `Ada.Real_Time.Timing_Events` è utile in tal senso.
      - [X] se il task perde il possesso del core a causa di
        -  ~~*preemption* da parte di un task *ready* a priorità più alta~~. Non può accadere in questo scenario in quanto esiste solo il task `P1`.
        -  esecuzione dell'istruzione di `delay_until`
        
        allora l'evento temporale deve essere **disarmato**. In generale, il disarmo deve avvenire ogni volta che il task perde/cede il core.

      - [X] se nel punto temporale assoluto `t+x` il task è ancora in esecuzione sul core, allora vuol dire che che il task sta eseguendo oltre il suo budget => `CPU_Budget_Exceeded`. Nella pratica, visto quanto detto al punto precedente, il `CPU_Budget_Exceeded` si verifica **solo ed esclusivamente** se il disarmo dell'evento temporale non avviene in tempo o non correttamente. "Dimenticare" un disarmo causerebbe la rilevazione di un `CPU_Budget_Exceeded` fasullo, cosa che chiaramente non deve accadere.  Il verificarsi di tale evento temporale corrisponde all'esecuzione dell'handler `CPU_BE_Detected`
