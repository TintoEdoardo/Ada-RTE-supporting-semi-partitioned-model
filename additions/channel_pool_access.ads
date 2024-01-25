------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   C H A N N E L _ P O O L _ A C C E S S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Finalization;
with Ada.Real_Time;
with Ada.Unchecked_Deallocation;
with Channel_Pool_Instances;
with Mixed_Criticality_System;

package Channel_Pool_Access is
   --  This package use generics.
   --  There is no limit on the number of
   --  channel pools that can be defined.

   type Channel_Levels is new Mixed_Criticality_System.Criticality;

   generic
      --  Element_Type is a generic element that
      --  can be allocatend as a message in a channel pool.
      type Element_Type is tagged private;

      --  Channel_First_Level  : Channel_Levels := HIGH;
      --  Channel_Second_Level : Channel_Levels := LOW;

   package Shared_Pointer is
      --  Instead of using standard access type,
      --  a shared pointer type is used.

      type Accessor (Element : access Element_Type) is limited private
        with Implicit_Dereference => Element;

      type Element_Type_Reference is limited private;

      --  We assume that just two criticality levels exist.
      --  In order to support more criticality levels, it
      --  should be possible to chose the desired pool.
      --  for Element_Type_Reference'Storage_Pool use
      --  Channel_Pool_Instances.High_Low_Channel_Pool;

      --  Reference_Type encapsulate the access
      --  type for an element of Element_Type.
      type Reference_Type is new Ada.Finalization.Limited_Controlled
      with record
         Element : Element_Type_Reference;
      end record;

      procedure Finalize (Reference : in out Reference_Type);

      ---------------------------------
      --  Channel type definition
      ---------------------------------

      protected type Shared_Reference is
         procedure Send    (Reference : in out Reference_Type);
         procedure Receive (Reference : in out Reference_Type);
      private
         Message_Arrival_Time : Ada.Real_Time.Time;
         Internal_Reference : Reference_Type;
      end Shared_Reference;

      ---------------------------------
      --  Operations on Reference_Type
      ---------------------------------

      function Get (Reference : Reference_Type)
                    return Accessor;

      procedure Move (Left  : in out Reference_Type;
                      Right : in out Reference_Type);

      --  Allocate an element of type Element_Type,
      --  referenced by Reference.
      procedure Allocate (Reference : in out Reference_Type);
      procedure Allocate (Reference    : in out Reference_Type;
                          From_Element : Element_Type);

      procedure Free (Reference : in out Reference_Type);

      --  Return the value of Initialization_List (reference.id).
      function Is_Null (Reference : Reference_Type)
                        return Boolean;

   private

      type Accessor (Element : access Element_Type) is null record;

      type Element_Type_Reference is access Element_Type;
      for Element_Type_Reference'Storage_Pool use
        Channel_Pool_Instances.High_Low_Channel_Pool;

      procedure Free_Element is new
        Ada.Unchecked_Deallocation
          (Element_Type, Element_Type_Reference);

   end Shared_Pointer;

end Channel_Pool_Access;
