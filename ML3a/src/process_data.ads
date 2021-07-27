
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package Process_Data is

   type Values_Array is array (Integer range <>) of Float;

   package Data_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Values_Array);
   type Data_List is new Data_Package.Vector with null record;

   package Labels_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Integer);
   type Labels_List is new Labels_Package.Vector with null record;

   type Data_Record is record
      Data   : Data_List;
      Labels : Labels_List;
   end record;

   function Data_Length (Data : Data_Record) return Integer;
   procedure Load_Data (Data_File : File_Type; Num_Features : Integer;
                        Data : out Data_Record);
   procedure Print_Data (Data : Data_List; Num_Features : Integer;
                         First : Integer := 1; Last : Integer := 10);
   procedure Print_Data_Item (Data : Data_List;  Num_Features : Integer;
                              Item : Integer);

end Process_Data;
