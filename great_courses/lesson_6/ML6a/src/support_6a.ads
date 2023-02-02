
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;

package Support_6A is

   type Data_Record is record
      Features : Integer_Array_List;
      Labels   : ML_Types.Integer_List;
   end record;

   type Dictionary_Record is record
      Key   : Unbounded_String;
      Value : Natural := 0;
   end record;

   package Dictionary_Package is new
     Ada.Containers.Doubly_Linked_Lists (Dictionary_Record);
   subtype Dictionary_List is Dictionary_Package.List;

   function Get_Data (File_Name : String; Dictionary : Dictionary_List)
                      return Data_Record;
   procedure Print_Bayes_Data
     (Classifier : Python.Module; CLF : Python_API.PyObject;
      Word_Dict  : Dictionary_List; Sentence : ML_Types.Indef_String_List);
   function Read_Vocabulary (File_Name : String) return Dictionary_List;
   function Tokenize (Data : String; Dictionary : Dictionary_List)
                      return Integer_Array;
   function Word_List  (Dictionary : Dictionary_List)
                        return ML_Types.Indef_String_List;

end Support_6A;
