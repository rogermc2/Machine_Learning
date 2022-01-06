
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Openml; use Openml;

package body OML_Tests is

   procedure Test_Data_Info is
      Dataset_Name    : constant String := "mnist_784";
      Version         : constant Integer := 1;
      Data_Info       : JSON_Value;
   begin
      Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version);
      Put_Line ("Data_Info type" & (Kind (Data_Info)'Image));
   end ;

end OML_Tests;
