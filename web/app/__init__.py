from flask import Flask, jsonify, request, session, redirect, abort, render_template
app = Flask(__name__)

@app.route("/")
def loadIndex():
    return render_template('index.html')

@app.route("/fileUploadAPI", methods=["POST"])
def uploadHandle():
    return jsonify({"result":False})
