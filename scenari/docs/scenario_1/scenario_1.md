# Scenario 1

Questo scenario, costruito a partire dallo [scenario 0](../scenario_0/scenario_0.md), formula ***solo una parte*** del modo con il quale devono essere tracciati i risultati di un esperimento.

- [ ] Il programma è lo stesso dello [scenario 0](../scenario_0/scenario_0.md), ma con delle modifiche al corpo dei task di tipo `Periodic_First_CPU` e alla procedura `Periodic_Tasks.Init`: esse, ad ogni iterazione del loro `loop`, dovranno stampare sulla porta seriale (UART) una breve stringa a piacere. Queste stringhe, dovranno essere catturate da un apposito programma sulla macchina host (ad esempio *[cutecom](https://gitlab.com/cutecom/cutecom)*) e salvate su di un file di log.
 


