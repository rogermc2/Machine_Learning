
with Ada.Characters.Handling;

package body Dataset_Utilities is

   function To_Lowercase (Text : String) return String is
      use Ada.Characters.Handling;
      LC : String := Text;
   begin
      for char in Text'First .. Text'Last loop
         if Is_Upper (Text (char)) and then
           Text (char) in 'A' .. 'Z' then
            LC (char) := To_Lower (Text (char));
         end if;
      end loop;

      return LC;

   end To_Lowercase;

end Dataset_Utilities;
