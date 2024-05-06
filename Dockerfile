# Use the official Python image from the Docker Hub
FROM python:3.11.8

# Set environment variables
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1

RUN pip install --upgrade pip

# Install system dependencies
RUN apt-get update \
    && apt-get install -y postgresql-client \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -rms /bin/bash ctf && chmod 777 /opt /run
# Set the working directory in the container
WORKDIR /ctf


RUN mkdir /ctf/static && mkdir /ctf/media && chown -R ctf:ctf /ctf && chmod 755 /ctf


# Install project dependencies
COPY requirements.txt /ctf/
RUN pip install --no-cache-dir -r requirements.txt --no-build-isolation

# Copy the project code into the container
COPY . /ctf/

# Expose port 8000 to the outside world
EXPOSE 8000

USER ctf
# Run the Django development server
CMD ["python", "manage.py", "runserver", "0.0.0.0:8000"]
