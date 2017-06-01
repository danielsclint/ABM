#//////////////////////////////////////////////////////////////////////////////
#////                                                                       ///
#//// Copyright INRO, 2016-2017.                                            ///
#//// Rights to use and modify are granted to the                           ///
#//// San Diego Association of Governments and partner agencies.            ///
#//// This copyright notice must be preserved.                              ///
#////                                                                       ///
#//// build_toolbox.py                                                      ///
#////                                                                       ///
#////     Generates an MTBX (Emme Modeller Toolbox), based on the structure ///  
#////     of the Python source tree.                                        ///
#////                                                                       ///
#////     Usage: build_toolbox.py [-s source_folder] [-p toolbox_path]      ///
#////                             [-t toolbox_title] [-n toolbox_namespace] ///
#////                                                                       ///
#////         [-p toolbox_path]: Specifies the name of the MTBX file.       ///
#////              If omitted,defaults to the name of the source            ///
#////              folder + '.mtbx'.                                        ///
#////         [-t toolbox_title]: The title of the MTBX file. If omitted,   ///
#////             defaults to the name of the source folder capitalized.    ///
#////         [-n toolbox_namespace]: The namespace for the toolbox.        ///
#////             If omitted, defaults to the name of the source folder.    ///
#////         [-s source_folder]: The location of the source code folder.   ///
#////             If omitted, defaults to the working directory.            ///
#////                                                                       ///
#////                                                                       ///
#////                                                                       ///
#//////////////////////////////////////////////////////////////////////////////

import os
import re
import imp
from datetime import datetime
import subprocess
import sqlite3.dbapi2 as sqllib

import inro.emme.desktop.app as _app
import inro.modeller as _m


def check_namespace(ns):
    if not re.match("^[a-zA-Z][a-zA-Z0-9_]*$", ns):
        raise Exception("Namespace '%s' is invalid" % ns)


def get_emme_version():
    emme_process = subprocess.Popen(['Emme', '-V'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output = emme_process.communicate()[0]
    return output.split(',')[0]


class BaseNode(object):
    def __init__(self, namespace, title):
        check_namespace(namespace)
        self.namespace = namespace 
        self.title = title       
        self.parent = None
        self.root = None
        self.children = []

    def add_folder(self, namespace):
        node = FolderNode(namespace, parent=self)
        self.children.append(node)
        return node
    
    def add_tool(self, script_path, namespace):
        try:
            node = ToolNode(namespace, script_path, parent=self)
        except Exception, e:
            print script_path, namespace
            print type(e), str(e)
            return None
        self.children.append(node)
        _tool_mod = imp.load_source(namespace, script_path)
        node.order = getattr(_tool_mod, "TOOLBOX_ORDER", None)
        title = getattr(_tool_mod, "TOOLBOX_TITLE", None)
        if title:
            node.title = title
        return node

    def sort(self):
        self.children.sort(key=lambda x: x.order)
        for child in self.children:
            if isinstance(child, FolderNode):
                child.sort()


class ElementTree(BaseNode):
    
    def __init__(self, namespace, title):
        super(ElementTree, self).__init__(namespace, title)
        self.next_element_id = 0
        self.element_id = self.next_id()
        self.begin = str(datetime.now())
        self.version = "Emme %s" % get_emme_version()
        self.root = self
    
    def next_id(self):
        self.next_element_id += 1
        return self.next_element_id


class FolderNode(BaseNode):
    
    def __init__(self, namespace, parent):
        title = namespace.replace("_", " ").capitalize()
        super(FolderNode, self).__init__(namespace, title)
        self.parent = parent
        self.root = parent.root
        self.element_id = self.root.next_id()

    @property
    def order(self):
        child_order = [child.order for child in self.children if child.order is not None]
        if child_order:
            return min(child_order)
        return None
    
class ToolNode():
    
    def __init__(self, namespace, script_path, parent):
        check_namespace(namespace)
        self.namespace = namespace
        self.title = namespace.replace("_", " ").capitalize()
        self.script = script_path + ".py"

        self.root = parent.root
        self.parent = parent
        self.element_id = self.root.next_id()

        self.code = ''
        self.extension = '.py'
        self.order = None

class MTBXDatabase():    
    FORMAT_MAGIC_NUMBER = 'B8C224F6_7C94_4E6F_8C2C_5CC06F145271'
    TOOLBOX_MAGIC_NUMBER = 'TOOLBOX_C6809332_CD61_45B3_9060_411D825669F8'
    CATEGORY_MAGIC_NUMBER = 'CATEGORY_984876A0_3350_4374_B47C_6D9C5A47BBC8'
    TOOL_MAGIC_NUMBER = 'TOOL_1AC06B56_6A54_431A_9515_0BF77013646F'
    
    def __init__(self, filepath, title):
        if os.path.exists(filepath): 
            os.remove(filepath)
        
        self.db = sqllib.connect(filepath)
        
        self._create_attribute_table()
        self._create_element_table()
        self._create_document_table()
        self._create_triggers()
        
        self._initialize_documents_table(title)

    def _create_attribute_table(self):
        sql = """CREATE TABLE attributes(
            element_id INTEGER REFERENCES elements(element_id),
            name VARCHAR,
            value VARCHAR,
            PRIMARY KEY(element_id, name));"""
        
        self.db.execute(sql)
    
    def _create_element_table(self):
        sql = """CREATE TABLE elements(
            element_id INTEGER PRIMARY KEY AUTOINCREMENT,
            parent_id INTEGER REFERENCES elements(element_id),
            document_id INTEGER REFERENCES documents(document_id),
            tag VARCHAR,
            text VARCHAR,
            tail VARCHAR);"""
        
        self.db.execute(sql)
    
    def _create_document_table(self):
        sql = """CREATE TABLE documents(
            document_id INTEGER PRIMARY KEY AUTOINCREMENT,
            title VARCHAR);"""
        
        self.db.execute(sql)
    
    def _create_triggers(self):
        sql = """CREATE TRIGGER documents_delete
            BEFORE DELETE on documents
            FOR EACH ROW BEGIN
                DELETE FROM elements WHERE document_id = OLD.document_id;
            END"""
            
        self.db.execute(sql)
        
        sql = """CREATE TRIGGER elements_delete
            BEFORE DELETE on elements
            FOR EACH ROW BEGIN
                DELETE FROM attributes WHERE element_id = OLD.element_id;
            END"""
        
        self.db.execute(sql)
    
    def _initialize_documents_table(self, title):
        sql = """INSERT INTO documents (document_id, title)
                VALUES (1, '%s');""" % title
        
        self.db.execute(sql)
        self.db.commit()

    def populate_tables_from_tree(self, tree):        
        
        #Insert into the elements table
        column_string = "element_id, document_id, tag, text, tail"
        value_string = "{id}, 1, '{title}', '', ''".format(
            id=tree.element_id, title=tree.title)
        sql = """INSERT INTO elements (%s)
                VALUES (%s);""" % (column_string, value_string)
        self.db.execute(sql)
        
        #Insert into the attributes table
        column_string = "element_id, name, value"
        atts = {'major': '',
                'format': MTBXDatabase.FORMAT_MAGIC_NUMBER,
                'begin': tree.begin,
                'version': tree.version,
                'maintenance': '',
                'minor': '',
                'name': tree.title,
                'description': '',
                'namespace': tree.namespace,
                MTBXDatabase.TOOLBOX_MAGIC_NUMBER: 'True'}
        for key, val in atts.iteritems():
            value_string = "{id}, '{name}', '{value}'".format(
                id=tree.element_id, name=key, value=val)
            sql = """INSERT INTO attributes (%s)
                    VALUES (%s);""" % (column_string, value_string)
            self.db.execute(sql)
        
        self.db.commit()
        
        #Handle children nodes
        for child in tree.children:
            if isinstance(child, ToolNode):
                self._insert_tool(child)
            else:
                self._insert_folder(child)
    
    def _insert_folder(self, node):
        #Insert into the elements table
        column_string = "element_id, parent_id, document_id, tag, text, tail"
        value_string = "{id}, {parent}, 1, '{title}', '', ''".format(
            id=node.element_id, parent=node.parent.element_id, title=node.title)
        sql = """INSERT INTO elements (%s)
                VALUES (%s);""" % (column_string, value_string)
        self.db.execute(sql)
        
        #Insert into the attributes table
        column_string = "element_id, name, value"
        atts = {'namespace': node.namespace,
                'description': '',
                'name': node.title,
                'children': [c.element_id for c in node.children],
                MTBXDatabase.CATEGORY_MAGIC_NUMBER: 'True'}
        for key, val in atts.iteritems():
            value_string = "{id}, '{name}', '{value}'".format(
                id=node.element_id, name=key, value=val)
            sql = """INSERT INTO attributes (%s)
                    VALUES (%s);""" % (column_string, value_string)
            self.db.execute(sql)
            
        self.db.commit()
        
        #Handle children nodes
        for child in node.children:
            if isinstance(child, ToolNode):
                self._insert_tool(child)
            else:
                self._insert_folder(child)
    
    def _insert_tool(self, node):
        #Insert into the elements table
        column_string = "element_id, parent_id, document_id, tag, text, tail"
        value_string = "{id}, {parent}, 1, '{title}', '', ''".format(
            id=node.element_id, parent=node.parent.element_id, title=node.title)
        
        sql = """INSERT INTO elements (%s)
                VALUES (%s);""" %(column_string, value_string)
        self.db.execute(sql)
        
        #Insert into the attributes table
        column_string = "element_id, name, value"
        atts = {'code': node.code,
                'description': '',
                'script': node.script,
                'namespace': node.namespace,
                'python_suffix': node.extension,
                'name': node.title,
                MTBXDatabase.TOOL_MAGIC_NUMBER: 'True'}
        for key, val in atts.iteritems():
            value_string = "{id}, '{name}', '{value!s}'".format(
                id=node.element_id, name=key, value=val)
            sql = """INSERT INTO attributes (%s)
                    VALUES (?, ?, ?);""" %column_string
            self.db.execute(sql, (node.element_id, key, val))
        
        self.db.commit()
#---
#---MAIN METHOD

def build_toolbox(toolbox_file, source_folder, title, namespace):
    print "------------------------"
    print " Build Toolbox Utility"
    print "------------------------"
    print ""
    print "toolbox: %s" % toolbox_file
    print "source folder: %s" % source_folder
    print "title: %s" % title
    print "namespace: %s" % namespace
    print ""
    
    print "Loading toolbox structure"
    tree = ElementTree(title, namespace)
    explore_source_folder(source_folder, tree)
    print "Done. Found %s elements." % (tree.next_element_id)
    tree.sort()
    
    print ""
    print "Building MTBX file."
    mtbx = MTBXDatabase(toolbox_file, title)
    mtbx.populate_tables_from_tree(tree)
    print "Done."
    
def explore_source_folder(root_folder_path, parent_node):
    folders = []
    files = []
    for item in os.listdir(root_folder_path):
        itempath = os.path.join(root_folder_path, item)
        if os.path.isfile(itempath):
            name, extension = os.path.splitext(item)
            if extension != '.py': 
                continue # skip non-Python files
            if os.path.normpath(itempath) == os.path.normpath(os.path.abspath(__file__)):
                continue # skip this file
            files.append((name, extension))
        else:
            folders.append(item)
    
    for foldername in folders:
        folderpath = os.path.join(root_folder_path, foldername)
        folder_node = parent_node.add_folder(namespace=foldername)
        explore_source_folder(folderpath, folder_node)
    
    for filename, ext in files:
        script_path = os.path.join(root_folder_path, filename + ext)
        parent_node.add_tool(script_path, namespace=filename)    

if __name__ == "__main__":
    '''
    Usage: build_toolbox.py [-p toolbox_path] [-t toolbox_title] [-n toolbox_namespace] [-s source_folder]
    '''
    desktop = _app.connect(port=4242)
    modeller = _m.Modeller(desktop)
    
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--src', help= "Path to the source code folder. Default is the working folder.")
    parser.add_argument('-p', '--path', help= "Output file path. Default is 'folder_name.mtbx' in the source code folder.")
    parser.add_argument('-t', '--title', help= "Title of the Toolbox. Default is 'Folder name' for the source code.")
    parser.add_argument('-n', '--namespace', help= "The initial namespace. Default is 'folder_name' for the source code.")
    
    args = parser.parse_args()

    source_folder = args.src or os.path.dirname(os.path.abspath(__file__))
    folder_name = os.path.split(source_folder)[1]
    title = args.title or folder_name.capitalize().replace("_", " ")
    toolbox_file = args.path or folder_name + ".mtbx"
    namespace = args.namespace or folder_name
    
    build_toolbox(toolbox_file, source_folder, title, namespace)