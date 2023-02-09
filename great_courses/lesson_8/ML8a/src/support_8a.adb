
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;

--  with Basic_Printing; use Basic_Printing;
with Neural_Utilities;

package body Support_8A is

   --  -------------------------------------------------------------------------

--     function Fit (Data : Real_Float_Matrix) return Real_Float_Vector is
--        Routine_Name : constant String := "Support_8A.Load_Data ";
--        Result            : Real_Float_Vector (Data'Range);
--     begin
--        for index in Data'Range loop
--           null;
--           Result (index) := Target (Data (index, 1));
--        end loop;
--
--        return Result;
--
--     end Fit;

   --  -------------------------------------------------------------------------

   function Load_Data (File_Name : String) return Data_Record is
--        File_ID  : File_Type;
--        Data  : Real_Float_Matrix (1 .. Num_Samples, 1 .. 1);
      Raw_Data : ML_Types.Raw_Data_Vector;
      Data     : Data_Record;
   begin
      Raw_Data := Neural_Utilities.Load_Raw_CSV_Data (File_Name);

      return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

end Support_8A;
