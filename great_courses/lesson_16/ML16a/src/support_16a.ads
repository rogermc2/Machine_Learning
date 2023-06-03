
with Ada.Containers.Doubly_Linked_Lists;
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

   type Dictionary_Record is record
      Key    : Unbounded_String;
      Values : NL_Types.Float_List;
   end record;

   type Newsgroups_Record is record
      Data         : ML_Types.Unbounded_List;
      Target       : Python_API.PyObject_Ptr;
      File_Names   : Python_API.PyObject_Ptr;
      Descr        : Python_API.PyObject_Ptr;
      Target_Names : Python_API.PyObject_Ptr;
   end record;

   package Dictionary_Package is new
     Ada.Containers.Doubly_Linked_Lists (Dictionary_Record);
   subtype Dictionary_List is Dictionary_Package.List;

   function ProbA_Chooser
     (Classifier       : Python.Module;
      Current_Item     : Positive; Num_Items : Positive;
      Labeled_Examples : Data_Items;
      Train_Set        : ML_Types.Integer_List_2D;
      Train_Labels     : ML_Types.Integer_List; Alpha : Integer;
      Clf              : Python_API.PyObject_Ptr) return Integer;
   function Get_Glove_Data (File_Name : String) return Dictionary_List;
   function Load_Newsgroups (Classifier : Python.Module; File_Name : String;
                       Reload : Boolean := False) return Newsgroups_Record;
--     function Read_Vocabulary (File_Name : String) return Dictionary_List;
   procedure Save_Data (Data : Newsgroups_Record; File_Name : String);

end Support_16A;
