--  From https://franckbehaghel.eu/programming/ada/ada-web-server-example/
--       http_req.php

with Ada.Text_IO;

package body Configure_AWS is

   procedure Get_Key_Value (S : in String; key, value : out Unbounded_String) is
      Index : Natural;
   begin
      Index := Fixed.Index (S, "=");
      if (Index = 0) then
         key   := To_Unbounded_String (S);
         value := To_Unbounded_String ("");
      else
         key   := To_Unbounded_String (S (S'First .. Index - 1));
         value := To_Unbounded_String (S (Index + 1 .. S'Last));
      end if;
   end Get_Key_Value;

   procedure Add_Param (Cfg : in out Config_Maps.Map; Arg : in String) is

      key, value : Unbounded_String;
   begin

      Get_Key_Value (Arg, key, value);
      Ada.Text_IO.Put_Line ("key" & To_String (key));
      Config_Maps.Insert (Container => Cfg, Key => key, New_Item => value);

   end Add_Param;

end Configure_AWS;
