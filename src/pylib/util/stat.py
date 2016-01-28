__author__ = 'mahandong'
import random
import csv, shutil, os, glob, cPickle
from log import strd_logger
import re

def weightedResample1(p, w):
    try:
        #p #original objects
        #w #weight vector
        if len(p) != len(w):
            return False
        else:
            N = len(p) #total length
        p3 = []
        index = int(random.random()* N)
        beta = 0.0
        for i in range(N):
            beta += random.random() * 2.0 * max(w)
            while beta > w[index]:
                beta -= w[index]
                index = (index + 1) % N
            p3.append(p[index])
        return p3
    except Exception as e:
        log.error(e)

def weightedResample2(p, w):
    try:
        #p #original objects
        #w #weight vector
        if len(p) != len(w):
            return False
        else:
            N = len(p) #total length
        w_norm = []
        for i in range(N):
            w_norm.append(w[i]/sum(w))
        w_cdf = []
        temp = 0
        for i in range(N):
            temp += w_norm[i]
            w_cdf.append(temp)
        p3 = []
        for i in range(N):
            seed = random.random()
            for j in range(N):
                if seed <= w_cdf[j]:
                    p3.append(p[j])
                    break
        return p3
    except Exception as e:
        log.error(e)