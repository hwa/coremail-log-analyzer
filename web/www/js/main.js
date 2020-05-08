/** @jsx React.DOM */

function overview(){
    $.getJSON('/overview',
             success = function(rsp){
                 $('#overview').append(JSON.stringify(rsp));
             });

    $.getJSON('/mounted',
              success = function(rsp){
                  $('#mounted').append(JSON.stringify(rsp));
              });

    $.getJSON('/warn',
              success = function(rsp){
                  $('#warn').append(JSON.stringify(rsp).replace(/</g,'&lt;').replace(/>/g,'&gt;'));
              });

}

var Overview = React.createClass({
    getInitialState: function(){
        this.getOverview();
        this.getMounted();
        return {'overview': {}}

    },

    getOverview: function(){
        $.getJSON('/overview',
              function(overview){
                  this.setState({'overview': overview});
              }.bind(this));
    },

    getMounted: function(){
        $.getJSON('/mounted',
                  function(mounted){
                      _.each(this.state.overview.mail_servers,
                             function(s){s['mounted'] = _.contains(mounted['mounted'], s['hostname'])});
                      this.forceUpdate();
                    }.bind(this));
    },

    render: function(){
        if (this.state.overview.mail_servers){
        var servers = this.state.overview.mail_servers.map(
            function(server){
                return <tr><td>{server.hostname}</td>
                           <td>{server.ip}</td>
                           <td>{server.mounted && "挂载到本地" || "卸载"}</td>
                       </tr>
            }
        )};
        return (
                <div>
                  <h2>概览</h2>
                <h3>配置</h3>
                <dl className="dl dl-horizontal">
                  <dt>mongo host</dt>
                  <dd>{this.state.overview.mongo_host}</dd>
                  <dt>mongo db</dt>
                  <dd>{this.state.overview.mongo_db}</dd>
                  <dt>diamond home dir</dt>
                  <dd>{this.state.overview.diamond_dir}</dd>
                  <dt>log fuse dir</dt>
                  <dd>{this.state.overview.logs_fuse_dir}</dd>
                </dl>

                <h3>邮件服务器</h3>
                <table>
                  <thead><td>机器名</td><td>IP</td><td>挂载</td></thead>
                  <tbody>
                    {servers}
                  </tbody>
                </table>

                </div>
        );
    }
});

var SentGraph = React.createClass({
    getInitialState: function(){
        return {'counts': {}};
    },

    getCounts: function(){
        $.getJSON('/sender/'+this.props.sender,
                 function(counts){
                     this.setState({'counts': counts});
                     this.renderSVG(counts);
                 }.bind(this));
    },

    draw: function(svg, data, coloring, sorting){
        if (_.isUndefined(coloring)){
            coloring = function(d){return null;};
        }

        if (! _.isUndefined(sorting)){
            data = _.sortBy(data, sorting);
        }

        var height = 200,
            width = 200,
            color = d3.scale.category10();

        var bubble = d3.layout.pack()
            .sort(null)
            .size([height, width])
            .padding(1);

        node_data = bubble.nodes({"children": data});

        var node = svg.selectAll(".node")
            .data(node_data.filter(function(d){return !d.children;}));

        node.enter().append('g')
            .attr('class', 'node')
            .attr('transform', function(d){return "translate(" + d.x + "," + d.y + ")";});

        node.append('circle')
            .attr('r', function(d){return d.r})
            .style("fill",function(d){return color(coloring(d));});

        node.append('title')
            .text(function(d){return d.key});

    },

    renderSVG: function(counts){
        var subjects = _.map(counts['subject'],
                         function(d){return {key: d.subject,
                                             value: d.c};});
        $('#subject-graph').empty();
        this.draw(d3.select("#subject-graph"), subjects);

        var domains = _.map(counts['to_domain'],
                            function(d){return {key: d.t,
                                                value: d.c};});
        $('#todomain-graph').empty();
        this.draw(d3.select('#todomain-graph'), domains);


        var rcpts = _.map(counts['to'],
                         function(d){return {key: d.to,
                                            value: d.c}});
        $('#rcpt-graph').empty();
        this.draw(d3.select('#rcpt-graph'), rcpts,
                  function(d){return d.key.match(/[^@]*@([^>]*)>/)[1];},
                  function(d){return d.key.match(/[^@]*@([^>]*)>/)[1];}
                 );


    },

    render: function(){
        //this.getCounts();
        return (<div >
                <div className="" className="btn-group" >
                 <button type="button" onClick={this.getCounts} className="btn btn-default">{this.props.sender}</button>
                 <button type="button" className="btn btn-default">封禁</button>
                </div>

                <div className="row">
                <div className="col-md-4">
                <h4>主题</h4>
                 <svg className="bubble" id="subject-graph" height="200" width="200"/>
                 <KVList key="subject" value="c" data={this.state.counts['subject']} />
                </div>
                <div className="col-md-4">
                <h4>收信域</h4>
                 <svg className="bubble" id="todomain-graph" height="200" width="200"/>
                 <KVList key="t" value="c" data={this.state.counts['to_domain']} />
                </div>
                <div className="col-md-4" height="200">
                <h4>收信人</h4>
                <svg className="bubble" id="rcpt-graph" height="200" width="200"/>
                <KVList key="to" value="c" data={this.state.counts['to']} />
                </div>

                </div>
                </div>
               );
    }
});

var KVList = React.createClass({
    render: function(){
        kf = this.props.key;
        vf = this.props.value;
        trs = _.map(this.props.data,
                    function(d){return (<tr>
                                        <td>{d[kf]}</td>
                                        <td>{d[vf]}</td>
                                        </tr>);});
        return (
                <table>
                {trs}
                </table>);
    }
});


var WarnSent = React.createClass({
    getInitialState: function(){
        return {'selected_sender': null};
    },

    onClickSender: function(e){
        t = $(e.target).text();
        this.setState({'selected_sender': t});
    },

    render: function(){
        var sents = _.map(_.sortBy(this.props.sent, function(s){return -s.count}),
                          function(s){
                              return <tr onClick={this.onClickSender}>
                                      <td>{s.mail_server}</td>
                                      <td>{s.from}</td>
                                      <td>{s.count}</td>
                                     </tr>
                          }.bind(this)
                         );
        return (
                <div className="row"><h3>超发账号</h3>
                  <div className="col-md-4">
                  <table>
                   {sents}
                  </table>
                  </div>
                <div className="col-md-8">
                <SentGraph sender={this.state.selected_sender} />
                </div>
                </div>
               );

    }
});

var WarnFailUser = React.createClass({
    render: function(){
        var fu = _.map(_.sortBy(this.props.failuser, function(s){return -s.count}),
                          function(s){
                              return <tr>
                                  <td>{s.mail_server}</td>
                                  <td>{s.user}</td>
                                  <td>{s.count}</td>
                                  </tr>
                          }
                         );
        return (
                <div><h3>口令错误账号</h3>
                <table>
                {fu}
            </table>
                </div>
        );

    }
});

var WarnFailIP = React.createClass({
    render: function(){
        var fi = _.map(_.sortBy(this.props.failip, function(s){return -s.count}),
                       function(s){
                           return <tr>
                               <td>{s.mail_server}</td>
                               <td>{s.ip}</td>
                               <td>{s.count}</td>
                               </tr>
                       }
                      );
        return (
                <div><h3>口令错误IP</h3>
                <table>
                {fi}
            </table>
                </div>
        );

    }
});

var Warn = React.createClass({
    getInitialState: function(){
        this.getWarn();
        window.setInterval(this.getWarn, 10000);
        return {'warn':{}}
    },

    getWarn: function(){
        $.getJSON('/warn',
                 function(warn){
                     this.setState({'warn': warn})
                 }.bind(this))
    },

    render : function(){
        return (<div>
                <WarnSent sent={this.state.warn.sent} />
                <WarnFailUser failuser={this.state.warn.failuser} />
                <WarnFailIP failip={this.state.warn.failip} />
                </div>
            );

    }
});

var App = React.createClass({

    render: function(){
        return (<div>
               <Overview />
               <Warn />
               </div>) ;



    }
});





function main(){
   // overview();
    React.renderComponent(<App />, document.getElementById('app'));

}

$(document).ready(main);
