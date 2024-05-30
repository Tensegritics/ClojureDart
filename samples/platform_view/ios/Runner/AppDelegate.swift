import Flutter
import UIKit

@UIApplicationMain
@objc class AppDelegate: FlutterAppDelegate {
    override func application(
        _ application: UIApplication,
        didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey : Any]?
    ) -> Bool {
        GeneratedPluginRegistrant.register(with: self)

        weak var registrar = self.registrar(forPlugin: "clojuredart_plugin")

        let factory = FLNativeViewFactory(messenger: registrar!.messenger())

        registrar!.register(
          factory,
          withId: "clojuredart_component")
        return super.application(application, didFinishLaunchingWithOptions: launchOptions)
    }
}
