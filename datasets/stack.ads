
with Ada.Containers.Vectors;

generic
   Size : Integer;
   type Elem is private;

   with procedure Print (E : in Elem);

package Stack is
   type Stack is limited private;

   subtype Count_Type is Integer range 0 .. Size;
   subtype Index_Type is Count_Type range 1 .. Size;
   package Stack_Vectors is new Ada.Containers.Vectors (Index_Type => Index_Type, Element_Type => Elem);

   procedure Push (S : in out Stack; E : in Elem);
   procedure Pop (S : in out Stack; E : out Elem);
   procedure Pop (S : in out Stack);
   procedure Pop_Until (S : in out Stack; E : in Elem);
   procedure Print_All (S : in Stack);
   function Length (S : in Stack) return Count_Type;
   OVERFLOW, UNDERFLOW : exception;

private
   type Stack is record
      Len : Count_Type := 0;
      V   : Stack_Vectors.Vector;
   end record;
end Stack;
