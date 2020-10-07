# Scenario 10

Torna all'[indice](../index.md).

In questo scenario, basato sullo [scenario 9](../scenario_9/scenario_9.md), ci si preoccupa, su **ambo** i core (CPU1 e CPU2), di:
- monitorare il consumo del *LO-Crit* budget dei task;
- rilevare gli eventuali `CPU_Budget_Exceeded` e, alla loro occorrenza, scrivere qualcosa di utile sulla porta seriale UART;

Bisogna, per ambo i core:
- [X] poter gestire il monitoraggio del consumo del *LO-Crit* budget anche sul secondo core;
  - [X] impostare il monitoraggio con il valore relativo al *LO-Crit* del task che sta per eseguire;
  - [X] avviare tale monitoraggio;
  - [X] cancellarlo qualora il task dovesse perdere la propria CPU **non** a causa di esaurimento del suo *LO-Crit* budget;
- [X] disattivare i cambi di criticità associati alla modalità di esecuzione dei core
  - la si attiverà in futuri scenari più complessi;
- [X] forzare un `CPU_Budget_Exceeded` su ambo i core sia:
  - [X] in maniera più "contemporanea" possibile;
  - [X] sia ad una distanza temporale più rilevante;
  - serve a capire se ci sono eventuali interferenze indesiderate fra la CPU1 e la CPU2;   
- [X] al verificarsi di un `CPU_Budget_Exceeded`:
  - [X] se è stato rilevato sulla CPU1, allora scrivere sulla porta seriale UART "`CPU1: LO-CRIT CPU_Budget_Exceeded DETECTED.`";
  - [X] se è stato rilevato sulla CPU2, allora scrivere sulla porta seriale UART "`CPU2: LO-CRIT CPU_Budget_Exceeded DETECTED.`";
  - [X] la CPU interessata deve bloccarsi 



Vai al [prossimo scenario](../scenario_11/scenario_11.md).\
Torna all'[indice](../index.md).