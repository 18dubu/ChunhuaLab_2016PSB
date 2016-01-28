__author__ = 'mahandong'

from pylib.util.file import *
from pylib.clinicaltrial.cde import *
import re

prepareData = 0

global diseaseNamePath
diseaseNamePath = '../result/diseaseNumber'

if prepareData == 1:

    dataRootPath = '../data/mental-disorder-years-threshold-3percent'
    group = "exclusion"
    outDir = "../result/" + group
    targetFile = "cde-" + group + ".csv" #cde-inclusion.csv


    #make diseaseNumber file from dir name
    dirname2list(dataRootPath, diseaseNamePath)

    #map Did and Disease Name (optional)
    mapDiseaseName(diseaseNamePath) # :return: dictionaries:Did2Name, Name2Did

    #gather all data to list
    mkdir(outDir)
    get_sep_col_2_list(dataRootPath,targetFile, 1, outDir+"/allCDE")
    get_sep_col_2_list(dataRootPath,targetFile,2, outDir+"/allFreq", 0)
    get_sep_col_2_list(dataRootPath,targetFile,3, outDir+"/allST")

#need preprocessing

    #norm ST
    sepLine2List(outDir + "/allST", " - ")

    #integrate separate files and create table
    integrateSepFiles(dataRootPath,targetFile, outDir+"/allData_table")

    #get combined CDE,ST file
    target = ["../result/exclusion/allST_sep","../result/inclusion/allST_sep"]
    combineFiles(target,'../result/allST_combine',1,0)
    target = ["../result/exclusion/allCDE","../result/inclusion/allCED"]
    combineFiles(target,'../result/allCDE_combine')

    #get CDE-ST table for all
    files = ['../result/exclusion/allData_table','../result/inclusion/allData_table']
    col = [3,5]
    get_cde_st_table(files,col,"../result/cde_st_table",1,1)

    #prepare for cytoscape file (from inpath files using R)
    inPath = "../result/Comparison_in_ex/topCEF/each/"
    outPath = "../result/Comparison_in_ex/topCEF/each/cytoscape/"
    cytoscapePrepare2(inPath,outPath)

    #generate the separated tables for different ST
    table = "../result/Comparison_in_ex/topCEF/each/cytoscape/combine_table"
    STs = ['mental or behavioral dysfunction','disease or syndrome','pharmacologic substance','organic chemical','finding','therapeutic or preventive procedure','body part, organ, or organ component','body location or region']
    cytoscapePrepare3(table, STs,'../result/Comparison_in_ex/topCEF/each/cytoscape/perST/')


    #pubmed run
    #try to match pubmed and disease names
    #cautious: after this step, manual modification is needed! Don't run the code without acknowledge of possible work!
    #dirnameSimilarity(dataRootPath,"../data/pubmed",'../result/match')

    #manual work needed

#################################################################################################
# #change file format and move it to data/ dir, as for the compare with the CT
original_path = "../result/match_pubmed/"
outPub = "../data/pubmed_matched/"
diseases = [include for include in os.listdir(original_path) if os.path.isdir(original_path+include)]
#
# for disease in diseases:
#     targetDir = check_dir(outPub)+disease
#     mkdir(targetDir)
#     path = check_dir(original_path+disease)
#     targetsInPath = [path+i for i in os.listdir(path)]
#     appendFiles1(targetsInPath,check_dir(targetDir)+'cde-pubmed.csv',2,3)
#
# #
dataRootPath = '../data/pubmed_matched'
group = "pubmed"
outDir = "../result/" + group
targetFile = "cde-" + group + ".csv" #cde-inclusion.csv
# #
#
# #make diseaseNumber file from dir name
# dirname2list(dataRootPath, diseaseNamePath)
#
#gather all data to list
# mkdir(outDir)
# get_sep_col_2_list(dataRootPath,targetFile, 1, outDir+"/allCDE")
# get_sep_col_2_list(dataRootPath,targetFile,2, outDir+"/allFreq", 0)
# get_sep_col_2_list(dataRootPath,targetFile,3, outDir+"/allST")
#
# #norm ST
# sepLine2List(outDir + "/allST", " - ")

# #integrate separate files and create table
# integrateSepFiles(dataRootPath,targetFile, outDir+"/allData_table")
#
# #
compare1 = 'inclusion'#'pubmed'
compare2 = 'exclusion'

# #get combined CDE,ST file
# target = ["../result/"+compare1+"/allST_sep","../result/"+compare2+"/allST_sep"]
# combineFiles(target,'../result/allST_combine_'+compare1+"_"+compare2,1,0)
# target = ["../result/"+compare1+"/allCDE","../result/"+compare2+"/allCDE"]
# combineFiles(target,'../result/allCDE_combine_'+compare1+'_'+compare2)
#
#get CDE-ST table for all
files = ["../result/"+compare1+"/allData_table","../result/"+compare2+"/allData_table"]
col = [3,5]
get_cde_st_table(files,col,"../result/cde_st_table_"+compare1+'_'+compare2,1,1) ###

#using R coded before the following steps

# #prepare for cytoscape file (from inpath files using R)
# inPath = "../result/Comparison_pubmed_ex/topCEF/each/"
# outPath = "../result/Comparison_pubmed_ex/topCEF/each/cytoscape/"
# cytoscapePrepare2(inPath,outPath)
#
# #generate the separated tables for different ST
# table = "../result/Comparison_pubmed_ex/topCEF/each/cytoscape/combine_table"
# STs = ['mental or behavioral dysfunction','disease or syndrome','pharmacologic substance','organic chemical','finding','therapeutic or preventive procedure','body part, organ, or organ component','body location or region']
# cytoscapePrepare3(table, STs,'../result/Comparison_pubmed_ex/topCEF/each/cytoscape/perST/')
#
