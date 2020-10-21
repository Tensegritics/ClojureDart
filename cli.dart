import 'dart:async' as a;
import 'dart:io' as io;
import 'dart:convert' as conv;
import 'package:vm_service/vm_service.dart' as vm;
import 'package:vm_service/vm_service_io.dart' as vms;
import 'package:vm_service/utils.dart' as vmutils;

class CodeReceiver {
  a.StreamSubscription<vm.Event> _sub;
  a.Completer<String> _completer;
  vm.VmService _service;

  CodeReceiver(this._service) {
    void receiv1(vm.Event e) {
      if (e.extensionKind == 'clojure.compiler.emit') {
        _completer.complete(e.extensionData.data['response']);
        _sub.pause();
      }
    }
    _sub = _service.onExtensionEvent.listen(receiv1, onDone: () {
        _completer.complete(null);
        _sub = null;
    });
    _sub.pause();
    _service.streamListen('Extension');
  }

  a.Future<String> receive() {
    if (_sub == null) return null;
    _completer = a.Completer();
    _sub.resume();
    return _completer.future;
  }
}

a.Future main() async {
  final serverUri = Uri.parse('http://127.0.0.1:57416/-RFvXZo80HQ=/');
  final wsUri = vmutils.convertToWebSocketUrl(serviceProtocolUrl: serverUri).toString();
  final service = await vms.vmServiceConnectUri(wsUri);
  final virtualMachine = await service.getVM();
  final mainIsolate = virtualMachine.isolates.first;

  a.Stream<String> cli = io.stdin.transform(conv.utf8.decoder);
  cli.listen((String s) async {
      await service.callServiceExtension('ext.clojure.read', isolateId: mainIsolate.id, args: {'stream': s});
  });

  service.onStdoutEvent.listen((vm.Event e) {
      io.stdout.write(conv.utf8.decode(conv.base64.decode(e.bytes)));
  });
  service.streamListen('Stdout');

  final codeReceiver = CodeReceiver(service);

  print("Clojure Dart v0.0.ε");
  try {
    while(true) {
      io.stdout.write("=> ");
      final dartCode = await codeReceiver.receive();
      final out = io.File("../../../flutter/training/first_usage/lib/evalexpr.dart").openWrite();
      out.write(dartCode);
      out.close();
      await service.callServiceExtension('s0.reloadSources', isolateId: mainIsolate.id);
      final response = (await service.callServiceExtension('ext.clojure.eval', isolateId: mainIsolate.id)).toJson();
      io.stdout.write(response['response']);
      //await Future.delayed(Duration(hours:2));
      io.stdout.write("\n");
    }
  } finally {
//    cli.close();
  }
}
