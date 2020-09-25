# Scenario 0
- [X] Solo il core0 è attivo, mentre il core1 è disattivato (che **non** vuol dire che è attivato e non ha del carico).
- [X] Esiste un solo task periodico, contenuto in un apposito package, **oltre** a quello di idle e al `main` del programma.
- [X] tale task viene schedulato correttamente sul core0, ovvero è in testa alla coda di pronti del core0. Il corpo del task avrà la seguente specifica:
```
task type Periodic_First_CPU
  (Pri    : System.Priority;
   Period : Positive) with CPU => 1
is
  pragma Priority (Pri);
end Periodic_First_CPU;
```
e il seguente corpo
```
task body Periodic_First_CPU is
   Next_Period : Ada.Real_Time.Time;
   Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
begin
   Next_Period := Ada.Real_Time.Clock + Period_To_Add;
   loop
      delay until Next_Period;
      Next_Period := Next_Period + Period_To_Add;
   end loop;
end Periodic_First_CPU;
```
- [X] Il `main` del programma sarà:
```
with Periodic_Tasks;

procedure Scenario_0 is
   pragma Priority (0);
begin
   Periodic_Tasks.Init;
end Scenario_0;
```
dove `Init` è:
```
   procedure Init is
   begin
      loop
         null;
      end loop;
   end Init;
```
- [X] Il periodo di un task periodico deve essere espresso in microsecondi.
- [X] Si deve allocare un task `P1` di tipo `Periodic_First_CPU` di priorità `10` e periodo `1_000_000` di microsecondi.
- [X] Essendo questo task di priorità strettamente maggiore di quella del main (`10 > 0`), la procedura `Init` deve eseguire, sul core0, solo ed esclusivamente quando il task `P1` è sospeso.
