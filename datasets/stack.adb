
with Ada.Text_IO; use Ada.Text_IO;

package body Stack is
   procedure Push (S : in out Stack; E : in Elem) is
   begin
      if S.Len = Size then
         raise OVERFLOW;
      end if;
      S.Len := S.Len + 1;
      Stack_Vectors.Append (S.V, E);
   end Push;

   procedure Pop (S : in out Stack; E : out Elem) is
   begin
      if S.Len = 0 then
         raise UNDERFLOW;
      end if;

      E := Stack_Vectors.Last_Element (S.V);
      Stack_Vectors.Delete_Last (S.V);
      S.Len := S.Len - 1;
   end Pop;

   -- pop until E is found
   procedure Pop_Until (S : in out Stack; E : in Elem) is
      Element : Elem;

      Nb_Deleted_Item : Ada.Containers.Count_Type := 0;
      Length          : Count_Type                := S.Len;
      Found           : Boolean                   := False;
      End_Of_Stack    : Boolean                   := False;
      Pop_Cursor      : Stack_Vectors.Cursor      := Stack_Vectors.Last (S.V);
      use Ada.Containers;

   begin

      while not Found and not End_Of_Stack loop

         if Length = 0 then
            End_Of_Stack := True;
         else
            Nb_Deleted_Item := Nb_Deleted_Item + 1;
            Length          := Length - 1;
            Element         := Stack_Vectors.Element (Pop_Cursor);
            if (E = Element) then
               Found := True;
            end if;
            Pop_Cursor := Stack_Vectors.Previous (Pop_Cursor);
         end if;
      end loop;

      if (Found) then

         Stack_Vectors.Delete_Last (S.V, Nb_Deleted_Item);
         S.Len := S.Len - Count_Type (Nb_Deleted_Item);
      else
         Put ("Pop_Until Error:");
         Print (E);
         Put ("  Stack :");
         Print_All (S => S);
         Put_Line ("");
      end if;
   end Pop_Until;

   procedure Pop (S : in out Stack) is
   begin
      if S.Len = 0 then
         raise UNDERFLOW;
      end if;
      Stack_Vectors.Delete_Last (S.V);
      S.Len := S.Len - 1;
   end Pop;

   procedure Print_All (S : in Stack) is
   begin
      if (S.Len > 0) then
         for I in 1 .. S.Len - 1 loop
            Print (Stack_Vectors.Element (S.V, I));
            Put (":");
         end loop;
         Print (Stack_Vectors.Element (S.V, S.Len));
      else
         Put ("Empty Stack");
      end if;

   end Print_All;

   function Length (S : in Stack) return Count_Type is
   begin
      return S.Len;
   end Length;

end Stack;
