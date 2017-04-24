from flask import Flask, jsonify, request, session, redirect, abort, render_template, url_for
from werkzeug.utils import secure_filename
import os

UPLOAD_FOLDER = './app/uploads/'
ALLOWED_EXTENSIONS = set(['txt', 'json', 'csv'])

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

@app.route("/")
def loadIndex():
    return render_template('index.html')

@app.route("/fileUploadAPI", methods=["POST"])
def uploadHandle():
    if request.method == 'POST':
        # check if the post request has the file part
        if 'file' not in request.files:
            return jsonify({"result":False, "error":"No File Found"})

        files = request.files.getlist('file')
        print files
        for file in files:
            if file.filename == '':
                return jsonify({"result":False, "error" : "No file selected"})

        for file in files:
            if file and allowed_file(file.filename):
                filename = secure_filename(file.filename)
                file.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
                return redirect(url_for('uploaded_file',filename=filename))
