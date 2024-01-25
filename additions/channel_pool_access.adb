------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   C H A N N E L _ P O O L _ A C C E S S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
------------------------------------------------------------------------------

package body Channel_Pool_Access is

   package body Shared_Pointer is

      ---------------------------------
      --  Operations on Reference_Type
      ---------------------------------

      function Get (Reference : Reference_Type)
                    return Accessor
      is
      begin
         return Accessor'(Element => Reference.Element.all'Access);
      end Get;

      procedure Move (Left  : in out Reference_Type;
                      Right : in out Reference_Type)
      is
      begin
         Free (Left);
         Left.Element  := Right.Element;
         Right.Element := null;
      end Move;

      procedure Allocate (Reference : in out Reference_Type)
      is
      begin
         Reference.Element := new Element_Type;
      end Allocate;

      procedure Allocate (Reference    : in out Reference_Type;
                          From_Element : Element_Type)
      is
      begin
         Reference.Element     := new Element_Type;
         Reference.Element.all := From_Element;
      end Allocate;

      procedure Free (Reference : in out Reference_Type)
      is
      begin
         Free_Element (Reference.Element);
         Reference.Element := null;
      end Free;

      function Is_Null (Reference : Reference_Type)
                        return Boolean
      is
      begin
         return Reference.Element = null;
      end Is_Null;

      procedure Finalize (Reference : in out Reference_Type)
      is
      begin
         if Reference.Element /= null then
            Free (Reference);
         end if;
      end Finalize;

      ---------------------------------
      --  Operations on Channel
      ---------------------------------
      protected body Shared_Reference is
         procedure Send (Reference : in out Reference_Type)
         is
         begin
            Move (Internal_Reference, Reference);
            Message_Arrival_Time := Ada.Real_Time.Clock;
         end Send;

         procedure Receive (Reference : in out Reference_Type)
         is
         begin
            Move (Reference, Internal_Reference);
         end Receive;
      end Shared_Reference;

   end Shared_Pointer;

end Channel_Pool_Access;
