
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with NL_Types;
with Python;
with Python_API;

package Support_16A is

   type Data_Items (Rows : Positive) is record
      Features : Integer_Array_List;
      Labels   : Integer_Matrix (1 .. Rows, 1 .. 1);
   end record;

   type Coeffs_Record is record
      Key    : Unbounded_String;
      Coeffs : NL_Types.Float_List;
   end record;

   type Embedding_Matrix_Type is array (integer range <>) of
     NL_Types.Float_List;

   type Newsgroups_Record is record
      Data         : ML_Types.Unbounded_List;
      Target       : Python_API.PyObject_Ptr;
      File_Names   : Python_API.PyObject_Ptr;
      Descr        : Python_API.PyObject_Ptr;
      Target_Names : Python_API.PyObject_Ptr;
   end record;

   use NL_Types;
   package Coeffs_Dictionary_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Float_List);
   subtype Coeffs_Dictionary is Coeffs_Dictionary_Package.Map;

   package Occurrences_Dictionary_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Integer);
   subtype Occurrences_Dictionary is Occurrences_Dictionary_Package.Map;

   function Prepare_Embedding_Matrix
     ( Embeddings_Index : Coeffs_Dictionary;
       Word_Index : Occurrences_Dictionary; Max_Words : Positive)
      return Embedding_Matrix_Type;
   function ProbA_Chooser
     (Classifier       : Python.Module;
      Current_Item     : Positive; Num_Items : Positive;
      Labeled_Examples : Data_Items;
      Train_Set        : ML_Types.Integer_List_2D;
      Train_Labels     : ML_Types.Integer_List; Alpha : Integer;
      Clf              : Python_API.PyObject_Ptr) return Integer;
   function Get_Glove_Data (File_Name : String) return Coeffs_Dictionary;
   function Load_Newsgroups (Classifier : Python.Module; File_Name : String;
                       Reload : Boolean := False) return Newsgroups_Record;
--     function Read_Vocabulary (File_Name : String) return Dictionary_List;
   procedure Save_Data (Data : Newsgroups_Record; File_Name : String);

end Support_16A;
