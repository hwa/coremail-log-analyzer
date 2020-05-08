#-*- coding: utf-8 -*-

from bottle import *
import pymongo
import json
from os import listdir
from bson.code import Code
import redis
import re


mc = pymongo.MongoClient()
db = mc['diamond']

wwwdir = "./www"

@get('/')
@view('index')
def index():
    return dict()

@get('/overview')
def overview():
    o = db['config'].find_one({'key': 'overview'},
                              fields={'_id': False})
    if o:
        return o['value']
    else:
        return dict()
@get('/mounted')
def mounted():
    config = overview()
    logs_dir = config['logs_fuse_dir']
    mail_servers = config['mail_servers']
    mounted_servers = [d for d in listdir(logs_dir)
                       if listdir(logs_dir+'/'+d)]
    return dict(mounted = mounted_servers)

@get('/logdb')
def logdb():
    config = overview()
    mail_servers = config['mail_servers']
    for s in mail_servers:
        s['mta'] = db['mta'].find({'mailserver': s['hostname']}).count()
        s['da'] = db['da'].find({'mailserver': s['hostname']}).count()
    return dict(servers = mail_servers)

@get('/warn')
def warn():
    r = redis.Redis(unix_socket_path='/var/run/redis/redis.sock')
    sent = [{'from':i,'count':int(j)} for (i,j) in r.hgetall('sender').iteritems()
            if int(j) > 100]
    failuser = [dict(user=i,count=int(j)) for (i,j) in r.hgetall('failuser').iteritems()
                if int(j) > 100]
    failip = [dict(ip=i,count=int(j)) for (i,j) in r.hgetall('failip').iteritems()
                   if int(j) > 100]
    return dict(sent = sent,
                failuser = failuser,
                failip = failip)

@get('/sender/:sender')
def sender_overview(sender):
    return {'subject': counts('da', {"from":sender}, 'subject'),
            'to': counts('da', {'from': sender}, 'to'),
            'to_domain': count_todomain(sender)}

def counts(collection, query, field):
    m = Code("""
             function(){
             emit((this.%s && this.%s.length >0) && this.%s || 'null', 1);
             }
             """ % (field, field, field))
    r = Code("""
             function(k, vs){
             return Array.sum(vs);
             }
             """)
    c = db[collection].map_reduce(m, r, "tmp_coll", query = query)
    return [{field: d['_id'], 'c': d['value']} for d in  c.find()]

def count_todomain(sender):
    m = Code("""
             function(){
             domain = this.to && this.to.match(/[^@]*@([^>]*)>/)[1] || 'null';
             emit(domain, 1);
             }
             """)
    r = Code("""
             function(k, vs){
             return Array.sum(vs);
             }
             """)
    c = db['da'].map_reduce(m, r, "tmp_coll", query = {"from": sender})
    return [{'t': d['_id'], 'c': d['value']} for d in  c.find()]

@get('/fromto')
def fromto():
    email = lambda s: re.search(r'<(.*)>.*', s).group(1)
    domain = lambda s : re.search(r'[^@]*@*(.*)', s).group(1)
    def count(ls):
        z = {}
        for ii in ls:
            i = (ii['from'], ii['to'])
            if i in z:
                z[i] += 1



            else:
                z[i] = 1
        return [{'from':i[0], 'to':i[1], 'v':j} for i,j in z.iteritems()]

    rs = db['da'].find({'flag':'da:Info', 'from':{'$exists':True}, 'state':'sent'},
                       limit=100000)
    emails = [{'from': email(d['from']),
               'to': email(d['to'])} for d in rs]
    domains = [{'from': domain(d['from']),
                'to': domain(d['to'])} for d in emails]
    return dict(emails = count(emails),
                domains = count(domains))

@get('/graph')
@view('graph')
def graph():
    return dict()

@route('<any:path>')
def default(any):
    return static_file(any, root=wwwdir)



if __name__ == '__main__':
    run(host='0.0.0.0', port=80, server='paste')
